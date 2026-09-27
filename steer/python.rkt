#lang racket/base
;; Python support (milestone XL1): facts come from Python's own stdlib `ast`/`compile`, run by a worker
;; script in the installed python3. Nothing about Python's grammar is re-implemented here, and nothing
;; is imported or executed: the worker only parses and compiles (compiling also finds semantic errors
;; such as `return` outside a function). Same pattern as api.rkt: an embedded script, a subprocess,
;; a timeout, a clear "skipped" when python3 is missing.
;;   syntax gate (T45): a SyntaxError becomes a located finding; for the shapes Python names precisely
;;   (a bracket never closed, an unmatched or mismatched closer, a missing colon) the worker also tries an
;;   edit and offers it only when the file compiles afterwards ("verified").
(require racket/list racket/string racket/port racket/path json
         "common.rkt" "syntax-check.rkt" "graph.rkt")
(provide python-gate-check python-available? run-python-worker python-find-anchor python-list-names
         py-extract py-extract-batch py-resolve-import)

(define worker-source #<<WORKER
import sys, ast, json, re, warnings
warnings.simplefilter("ignore")

CLOSER = {"(": ")", "[": "]", "{": "}"}


def try_compile(src, name):
    try:
        tree = ast.parse(src, name)
        compile(tree, name, "exec", dont_inherit=True)
        return tree, None
    except (SyntaxError, ValueError, RecursionError, MemoryError) as e:
        return None, e


def code_end_col(line):
    """1-based column just after the last code character (before a trailing # comment and whitespace)."""
    line = line.rstrip("\r")
    quote = None
    i = 0
    end = len(line)
    while i < len(line):
        c = line[i]
        if quote:
            if c == "\\":
                i += 2
                continue
            if c == quote:
                quote = None
        elif c in "'\"":
            quote = c
        elif c == "#":
            end = i
            break
        i += 1
    return len(line[:end].rstrip()) + 1


def apply_edit(src, e):
    lines = src.split("\n")
    at = sum(len(l) + 1 for l in lines[: e["line"] - 1]) + e["col"] - 1
    if e["op"] == "insert":
        return src[:at] + e["text"] + src[at:]
    if e["op"] == "delete":
        return src[:at] + src[at + 1:]
    return src[:at] + e["text"] + src[at + 1:]


def scan_unclosed(src):
    """Brackets still open at end of file, outermost first: [(char, line, col)], strings and comments skipped."""
    stack = []
    i, n, line, col0 = 0, len(src), 1, 0
    while i < n:
        c = src[i]
        if c == "\n":
            line += 1; col0 = 0; i += 1
        elif c == "#":
            while i < n and src[i] != "\n":
                i += 1; col0 += 1
        elif c == "\\":
            if i + 1 < n and src[i + 1] == "\n":
                line += 1; col0 = 0
            else:
                col0 += 2
            i += 2
        elif c in "'\"":
            delim = c * 3 if src.startswith(c * 3, i) else c
            j = i + len(delim)
            while j < n:
                if src[j] == "\\":
                    j += 2; continue
                if src.startswith(delim, j):
                    j += len(delim); break
                if src[j] == "\n" and len(delim) == 1:
                    break
                j += 1
            seg = src[i:j]
            nl = seg.count("\n")
            if nl:
                line += nl; col0 = len(seg) - seg.rfind("\n") - 1
            else:
                col0 += len(seg)
            i = j
        else:
            if c in CLOSER:
                stack.append((c, line, col0 + 1))
            elif c in ")]}" and stack:
                stack.pop()
            i += 1; col0 += 1
    return stack


def indent_of(line):
    return len(line) - len(line.lstrip(" \t"))


def is_blank_or_comment(line):
    s = line.strip()
    return s == "" or s.startswith("#")


def verified(src, name, edit):
    tree, err = try_compile(apply_edit(src, edit), name)
    return err is None


def trim_ws_left(line, col):
    """Move an insertion column left over whitespace, so `url as x` becomes `url) as x`, not `url )as x`."""
    while col > 1 and line[col - 2] in " \t":
        col -= 1
    return col


def open_stack_before(src, lineno, col):
    """Brackets open just before (lineno, col), outermost first."""
    lines = src.split("\n")
    prefix = "\n".join(lines[: lineno - 1] + [lines[lineno - 1][: col - 1]]) if lineno <= len(lines) else src
    return scan_unclosed(prefix)


def candidates(src, err):
    """Candidate edits near the error, most plausible first. Each is verified by compiling before it is offered."""
    msg = getattr(err, "msg", "") or ""
    lines = src.split("\n")
    lineno, offset = getattr(err, "lineno", None), getattr(err, "offset", None)
    out = []
    if "was never closed" in msg:
        stack = scan_unclosed(src)
        if not stack:
            return out
        closers = "".join(CLOSER[c] for c, _l, _c in reversed(stack))
        outer_line = stack[0][1]
        base = indent_of(lines[outer_line - 1])
        end = len(lines)
        for ln in range(outer_line + 1, len(lines) + 1):
            if not is_blank_or_comment(lines[ln - 1]) and indent_of(lines[ln - 1]) <= base:
                end = ln - 1
                break
        # earliest first: a header's `)` goes before its colon, a call's at the end of its last line
        for ln in range(outer_line, min(end, outer_line + 40) + 1):
            line = lines[ln - 1].rstrip("\r")
            if is_blank_or_comment(line):
                continue
            col = code_end_col(line)
            if line[: col - 1].endswith(":"):
                out.append({"op": "insert", "line": ln, "col": col - 1, "text": closers})
            out.append({"op": "insert", "line": ln, "col": col, "text": closers})
    elif msg.startswith("unmatched '") and lineno and offset:
        out.append({"op": "delete", "line": lineno, "col": offset, "text": lines[lineno - 1][offset - 1: offset]})
    elif "does not match opening parenthesis" in msg and lineno and offset:
        m = re.search(r"opening parenthesis '(.)'", msg)
        if m:
            col = trim_ws_left(lines[lineno - 1], offset)
            first_on_line = lines[lineno - 1][: offset - 1].strip() == ""
            if first_on_line:                       # `}` starts its line: the missing `]` ends the previous code line
                prev = lineno - 1
                while prev >= 1 and is_blank_or_comment(lines[prev - 1]):
                    prev -= 1
                if prev >= 1:
                    out.append({"op": "insert", "line": prev, "col": code_end_col(lines[prev - 1]), "text": CLOSER[m.group(1)]})
            out.append({"op": "insert", "line": lineno, "col": col, "text": CLOSER[m.group(1)]})   # a closer is missing before this one
            out.append({"op": "replace", "line": lineno, "col": offset, "text": CLOSER[m.group(1)]})  # or this one is the wrong kind
    elif msg == "expected ':'" and lineno:
        out.append({"op": "insert", "line": lineno, "col": code_end_col(lines[lineno - 1]), "text": ":"})
    elif (msg.startswith("invalid syntax") or "expecting" in msg) and lineno and offset:
        stack = open_stack_before(src, lineno, offset)
        if stack:
            col = trim_ws_left(lines[lineno - 1], offset)
            out.append({"op": "insert", "line": lineno, "col": col, "text": CLOSER[stack[-1][0]]})
    return out


def suggest(src, name, err):
    """The first candidate edit after which the file compiles, else None. Bounded in time."""
    import time
    deadline = time.monotonic() + 2.0
    for e in candidates(src, err):
        if time.monotonic() > deadline:
            return None
        if verified(src, name, e):
            return e
    return None


def classify(msg):
    if "was never closed" in msg:
        return "unclosed-form"
    if msg.startswith("unmatched '"):
        return "extra-closer"
    if "does not match opening parenthesis" in msg:
        return "mismatched-closer"
    return "read-error"


def check(src, name):
    tree, err = try_compile(src, name)
    if err is None:
        return {"ok": True, "statements": len(tree.body)}
    out = {"ok": False, "type": type(err).__name__, "msg": getattr(err, "msg", None) or str(err),
           "lineno": getattr(err, "lineno", None), "offset": getattr(err, "offset", None),
           "end_lineno": getattr(err, "end_lineno", None), "end_offset": getattr(err, "end_offset", None)}
    out["kind"] = classify(out["msg"])
    if isinstance(err, SyntaxError):
        e = suggest(src, name, err)
        if e:
            e["verified"] = True
            out["edit"] = e
    return out


# ---------------------------------------------------------------------------------------------
# anchors (T49): qualified-name resolution, decorator-inclusive spans, a datum-shaped hash.

class QualifiedVisitor(ast.NodeVisitor):
    """Every def/class, keyed by its qualified dotted name ("Outer.Inner.method"). First-match wins
    per name, matching srcread.rkt's Racket behaviour; a name that appears more than once at the same
    qualification (overloads, singledispatch registrations, several __init__ in nested scopes reached
    by the same path) keeps only the first, and later ones are visible in `duplicates`."""

    def __init__(self):
        self.defs = {}      # qualname -> node
        self.duplicates = {}  # qualname -> [nodes after the first]
        self.stack = []

    def _enter(self, node, name):
        qual = ".".join(self.stack + [name])
        if qual in self.defs:
            self.duplicates.setdefault(qual, []).append(node)
        else:
            self.defs[qual] = node
        self.stack.append(name)
        self.generic_visit(node)
        self.stack.pop()

    def visit_FunctionDef(self, node):
        self._enter(node, node.name)

    visit_AsyncFunctionDef = visit_FunctionDef

    def visit_ClassDef(self, node):
        self._enter(node, node.name)


def def_span(node):
    """(start_line, end_line): from the first decorator (if any) through the body's last line."""
    start = node.lineno
    for d in getattr(node, "decorator_list", []) or []:
        start = min(start, d.lineno)
    return start, node.end_lineno


def normalize_for_hash(node):
    """The node with attributes (line/col) stripped, so only structure and text matter: comments and
    formatting are already invisible (they are not in the AST); this also drops docstring position but
    keeps the docstring's own text, matching the Racket datum hash's "content, not layout" rule."""
    return ast.dump(node, include_attributes=False, annotate_fields=True)


def find_anchor(src, name, qualname):
    tree, err = try_compile(src, "<anchor>")
    if err is not None:
        return {"found": False, "problem": "unreadable: " + (getattr(err, "msg", None) or str(err))}
    v = QualifiedVisitor()
    v.visit(tree)
    node = v.defs.get(qualname)
    if node is None:
        # ambiguity: the bare name exists, just not at this qualification, or only as a duplicate
        bare_hits = [q for q in v.defs if q == qualname or q.rsplit(".", 1)[-1] == name]
        return {"found": False, "problem": "no definition of " + qualname,
                "candidates": sorted(set(bare_hits))[:8]}
    start, end = def_span(node)
    dup_count = len(v.duplicates.get(qualname, []))
    result = {"found": True, "line": start, "end": end, "hash": short_hash(normalize_for_hash(node)),
              "kind": type(node).__name__.replace("AsyncFunctionDef", "async def").replace("FunctionDef", "def").replace("ClassDef", "class")}
    if dup_count:
        result["shadowed"] = dup_count   # this many later definitions share the same qualified name
    return result


def short_hash(s):
    import hashlib
    return hashlib.sha1(s.encode("utf-8")).hexdigest()[:12]


# ---------------------------------------------------------------------------------------------
# extract (T62): the graph-ir contract. One file-facts dict per input file; batched over stdin so a
# whole project costs one process, not one per file.

def _decorator_name(d):
    if isinstance(d, ast.Name):
        return d.id
    if isinstance(d, ast.Attribute):
        return d.attr
    if isinstance(d, ast.Call):
        return _decorator_name(d.func)
    return None


def _mkref(kind, name, receiver, scope, arity, line):
    return {"kind": kind, "name": name, "receiver": receiver, "scope": scope, "arity": arity, "line": line}


def _collect_calls(node, scope, out):
    """Call/decorator-style refs inside `node`'s own body only - a nested def/class's body is scanned
    at ITS OWN scope when the outer Extractor visits it, not here (no double-counting, no wrong scope)."""

    class Collector(ast.NodeVisitor):
        def visit_FunctionDef(self, n):
            pass

        visit_AsyncFunctionDef = visit_FunctionDef

        def visit_ClassDef(self, n):
            pass

        def visit_Call(self, n):
            fn = n.func
            arity = len(n.args)
            if isinstance(fn, ast.Name):
                out.append(_mkref("call", fn.id, None, scope, arity, n.lineno))
            elif isinstance(fn, ast.Attribute):
                recv = None
                base = fn.value
                if isinstance(base, ast.Name) and base.id == "self":
                    recv = "self"
                elif isinstance(base, ast.Call) and isinstance(base.func, ast.Name) and base.func.id == "super":
                    recv = "base"
                # anything else (a plain object, a module alias, ClassName.method()) is left with no
                # receiver hint: the linker's generic name-based resolution chain handles it, and Python
                # attribute calls on an arbitrary object are inherently dynamic anyway (note 12/16).
                out.append(_mkref("call", fn.attr, recv, scope, arity, n.lineno))
            self.generic_visit(n)

    Collector().generic_visit(node)


def _signature_of(node):
    if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)):
        try:
            args_src = ast.unparse(node.args)
        except Exception:
            args_src = ""
        prefix = "async def" if isinstance(node, ast.AsyncFunctionDef) else "def"
        return prefix + " " + node.name + "(" + args_src + ")"
    if isinstance(node, ast.ClassDef):
        try:
            bases_src = ", ".join(ast.unparse(b) for b in node.bases)
        except Exception:
            bases_src = ""
        return "class " + node.name + ("(" + bases_src + ")" if bases_src else "")
    return node.name


def _mkdef(kind, name, qualname, scope, node, shape, bases, decorators, entry_lines):
    start, end = def_span(node)
    return {"kind": kind, "name": name, "qualname": qualname, "scope": scope, "line": start, "end": end,
            "shape": shape, "hash": short_hash(normalize_for_hash(node)), "bases": bases,
            "decorators": [d for d in decorators if d], "entry": start in entry_lines, "exported": True}


class Extractor(ast.NodeVisitor):
    """One pass, keeping a dotted-name stack (like QualifiedVisitor) plus a parallel kind stack so a
    function directly inside a class is a 'method' (or 'constructor' for __init__), and nested
    functions/classes are still visited (and their OWN calls scanned) even though Racket's local-
    shadowing subtraction has no Python equivalent here (LEGB scoping is genuinely more involved; this
    stays an intentional may-call over-approximation, never a false claim of precision - notes/12/16)."""

    def __init__(self, entry_lines):
        self.stack = []
        self.kind_stack = []
        self.defs = []
        self.refs = []
        self.entry_lines = entry_lines

    def scope(self):
        return ".".join(self.stack) if self.stack else None

    def qual(self, name):
        return ".".join(self.stack + [name])

    def _decorator_refs(self, node):
        for d in node.decorator_list:
            dn = _decorator_name(d)
            if dn:
                self.refs.append(_mkref("decorates", dn, None, self.scope(), None, d.lineno))

    def visit_ClassDef(self, node):
        qn = self.qual(node.name)
        try:
            bases = [ast.unparse(b) for b in node.bases]
        except Exception:
            bases = []
        decorators = [_decorator_name(d) for d in node.decorator_list]
        self._decorator_refs(node)
        self.defs.append(_mkdef("class", node.name, qn, self.scope(), node, _signature_of(node), bases, decorators, self.entry_lines))
        _collect_calls(node, qn, self.refs)
        self.stack.append(node.name)
        self.kind_stack.append("class")
        for child in node.body:
            if isinstance(child, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)):
                self.visit(child)
        self.stack.pop()
        self.kind_stack.pop()

    def visit_FunctionDef(self, node):
        self._function(node)

    def visit_AsyncFunctionDef(self, node):
        self._function(node)

    def _function(self, node):
        qn = self.qual(node.name)
        in_class = bool(self.kind_stack) and self.kind_stack[-1] == "class"
        kind = "constructor" if (in_class and node.name == "__init__") else ("method" if in_class else "function")
        decorators = [_decorator_name(d) for d in node.decorator_list]
        self._decorator_refs(node)
        self.defs.append(_mkdef(kind, node.name, qn, self.scope(), node, _signature_of(node), [], decorators, self.entry_lines))
        _collect_calls(node, qn, self.refs)
        self.stack.append(node.name)
        self.kind_stack.append("function")
        for child in node.body:
            if isinstance(child, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)):
                self.visit(child)
        self.stack.pop()
        self.kind_stack.pop()


def _all_names(node):
    """The list of string literals in `__all__ = [...]` / `__all__ = (...)`, or None if not that shape."""
    if not (isinstance(node, ast.Assign) and len(node.targets) == 1
            and isinstance(node.targets[0], ast.Name) and node.targets[0].id == "__all__"):
        return None
    v = node.value
    if isinstance(v, (ast.List, ast.Tuple)):
        return [e.value for e in v.elts if isinstance(e, ast.Constant) and isinstance(e.value, str)]
    return None


def extract_file(src, path, entry_lines):
    tree, err = try_compile(src, path)
    if err is not None:
        return {"path": path, "lang": "python", "defs": [], "refs": [], "imports": [],
                "has_statements": False, "content_hash": short_hash(src)}
    ex = Extractor(set(entry_lines))
    ex.visit(tree)
    _collect_calls(tree, None, ex.refs)   # top-level (module scope) calls
    imports = []
    has_stmt = False
    all_list = None
    for node in tree.body:
        if isinstance(node, ast.Import):
            for alias in node.names:
                imports.append({"spec": alias.name, "alias": alias.asname, "line": node.lineno})
        elif isinstance(node, ast.ImportFrom):
            level = node.level or 0
            spec = ("." * level) + (node.module or "")
            imports.append({"spec": spec, "alias": None, "line": node.lineno})
        elif isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)):
            pass
        elif isinstance(node, ast.Expr) and isinstance(node.value, ast.Constant) and isinstance(node.value.value, str):
            pass  # a bare docstring
        else:
            names = _all_names(node)
            if names is not None:
                all_list = names
            else:
                has_stmt = True
    for d in ex.defs:
        if d["scope"] is None:
            d["exported"] = (d["name"] in all_list) if all_list is not None else (not d["name"].startswith("_"))
    return {"path": path, "lang": "python", "defs": ex.defs, "refs": ex.refs, "imports": imports,
            "has_statements": has_stmt, "content_hash": short_hash(src)}


def main():
    cmd = sys.argv[1]
    name = sys.argv[2] if len(sys.argv) > 2 else "<string>"
    if cmd == "syntax":
        src = sys.stdin.buffer.read().decode("utf-8", "replace")
        print(json.dumps(check(src, name)))
    elif cmd == "find":
        qualname = sys.argv[3]
        src = sys.stdin.buffer.read().decode("utf-8", "replace")
        print(json.dumps(find_anchor(src, qualname.rsplit(".", 1)[-1], qualname)))
    elif cmd == "names":
        src = sys.stdin.buffer.read().decode("utf-8", "replace")
        tree, err = try_compile(src, name)
        if err is not None:
            print(json.dumps({"error": getattr(err, "msg", None) or str(err)}))
        else:
            v = QualifiedVisitor()
            v.visit(tree)
            print(json.dumps(sorted(v.defs.keys())))
    elif cmd == "extract":
        payload = json.loads(sys.stdin.buffer.read().decode("utf-8", "replace"))
        for item in payload:
            try:
                result = extract_file(item["text"], item["path"], item.get("entry_lines", []))
            except Exception as e:
                result = {"path": item.get("path"), "lang": "python", "defs": [], "refs": [], "imports": [],
                          "has_statements": False, "content_hash": "", "error": str(e)}
            print(json.dumps(result))
    else:
        print(json.dumps({"error": "unknown command " + cmd}))


main()
WORKER
  )

;; ---------------------------------------------------------------------------------------------
;; Running the worker

(define (python-exe)
  (or (getenv "STEER_PYTHON")
      (let ([p (or (find-executable-path "python3") (find-executable-path "python"))]) (and p (path->string p)))))

(define (python-available?) (and (python-exe) #t))

;; → the worker's JSON as a hasheq, or #f when python is missing, times out, or fails to run.
;; extra-args (e.g. a qualified name for "find") are passed after `name` on the worker's command line.
(define (run-python-worker cmd text name #:timeout [timeout 15] . extra-args)
  (define exe (python-exe))
  (and exe
       (with-handlers ([exn:fail? (λ (e) #f)])
         (define-values (p out in err) (apply subprocess #f #f #f exe "-I" "-S" "-c" worker-source cmd name extra-args))
         (write-string text in)
         (close-output-port in)
         (define out-text (box ""))
         (define t1 (thread (λ () (set-box! out-text (port->string out)))))
         (define t2 (thread (λ () (port->string err))))
         (define done? (sync/timeout timeout p))
         (unless done? (subprocess-kill p #t))
         (subprocess-wait p)
         (thread-wait t1) (thread-wait t2)
         (close-input-port out) (close-input-port err)
         (and done? (eqv? (subprocess-status p) 0)
              (string->jsexpr (unbox out-text))))))

;; ---------------------------------------------------------------------------------------------
;; Syntax gate

(define (skipped file msg) (list (finding 'info 'skipped msg #:file file)))

;; → (values findings statement-count lang), the shape every gate returns (lang.rkt)
(define (python-gate-check text file)
  (cond
    [(not (python-available?))
     (values (skipped file "python3 not found: Python files are not checked (install Python, or set STEER_PYTHON)") #f "python")]
    [else
     (define r (run-python-worker "syntax" text file))
     (cond
       [(not r) (values (skipped file "the Python syntax check did not run (python3 failed or timed out)") #f "python")]
       [(hash-ref r 'ok #f) (values '() (hash-ref r 'statements 0) "python")]
       [else
        (define e (hash-ref r 'edit #f))
        (define edit (and e (hasheq 'op (string->symbol (hash-ref e 'op)) 'line (hash-ref e 'line) 'col (hash-ref e 'col)
                                    'text (hash-ref e 'text) 'verified #t)))
        (values (list (finding 'error (string->symbol (hash-ref r 'kind "read-error"))
                               (format "~a" (hash-ref r 'msg))
                               #:file file #:line (hash-ref r 'lineno #f) #:col (hash-ref r 'offset #f)
                               #:fix (and edit (string-append (edit-text edit) " (verified: the file compiles after this edit)"))
                               #:edit edit))
                0 "python")])]))


;; ---------------------------------------------------------------------------------------------
;; Anchor resolution: file.py#Class.method (any dotted qualification the ast can see).
;; → hasheq: found? line end hash problem candidates kind shadowed (same keys resolve-anchor uses)

(define (python-find-anchor text qualname)
  (cond
    [(not (python-available?)) (hasheq 'found? #f 'problem "python3 not found: cannot resolve Python anchors")]
    [else
     (define r (run-python-worker "find" text "<anchor>" qualname))
     (cond
       [(not r) (hasheq 'found? #f 'problem "the Python anchor lookup did not run (python3 failed or timed out)")]
       [(hash-ref r 'found #f)
        (hasheq 'found? #t 'line (hash-ref r 'line) 'end (hash-ref r 'end) 'hash (hash-ref r 'hash)
                'kind (hash-ref r 'kind) 'shadowed (hash-ref r 'shadowed #f))]
       [else (hasheq 'found? #f 'problem (hash-ref r 'problem "not found") 'candidates (hash-ref r 'candidates '()))])]))

;; Every qualified name (Class.method) defined in the file, for a did-you-mean suggestion when a
;; requested name is not found. '() when python3 is missing or the file does not parse.
(define (python-list-names text)
  (cond
    [(not (python-available?)) '()]
    [else
     (define r (run-python-worker "names" text "<anchor>"))
     (if (and r (list? r)) r '())]))

;; ---------------------------------------------------------------------------------------------
;; extract (T62): the graph-ir contract. Batched (one process for a whole project) rather than one
;; process per file, since T68's measurement runs this over real, many-file repos.

;; `;; steer: entry` -> `# steer: entry` for Python; same "the line right after the comment" contract
;; Racket's rkt-extract uses.
(define entry-comment-rx #px"#\\s*steer:\\s*entry\\s*$")
(define (python-entry-lines text)
  (define lines (string-split text "\n" #:trim? #f))
  (for/list ([l lines] [i (in-naturals 1)] #:when (regexp-match? entry-comment-rx l)) (add1 i)))

;; the `json` library decodes JSON null as the symbol 'null (via (json-null)); every optional field
;; the worker may send as null needs that turned back into #f before it reaches a graph.rkt struct.
(define (un-null v) (if (eq? v 'null) #f v))

(define (jsexpr->def h)
  (def (string->symbol (hash-ref h 'kind)) (hash-ref h 'name) (hash-ref h 'qualname) (un-null (hash-ref h 'scope #f))
       (hash-ref h 'line) (hash-ref h 'end) (un-null (hash-ref h 'shape #f)) (hash-ref h 'hash)
       (hash-ref h 'bases '()) (hash-ref h 'decorators '()) (hash-ref h 'entry #f) (hash-ref h 'exported #t)))

(define (jsexpr->ref h)
  (ref (string->symbol (hash-ref h 'kind)) (hash-ref h 'name)
       (let ([r (un-null (hash-ref h 'receiver #f))]) (and r (string->symbol r)))
       (un-null (hash-ref h 'scope #f)) (un-null (hash-ref h 'arity #f)) (hash-ref h 'line 0)))

(define (jsexpr->import h) (import (hash-ref h 'spec) (un-null (hash-ref h 'alias #f)) (hash-ref h 'line 0)))

(define (jsexpr->file-facts h)
  (file-facts (hash-ref h 'path) 'python (map jsexpr->def (hash-ref h 'defs '()))
              (map jsexpr->ref (hash-ref h 'refs '())) (map jsexpr->import (hash-ref h 'imports '()))
              (hash-ref h 'has_statements #f) (hash-ref h 'content_hash "")))

;; → (listof jsexpr), one per input item, or #f on failure. items: (listof (list path text entry-lines)).
(define (run-python-worker-extract items #:timeout [timeout 60])
  (define exe (python-exe))
  (and exe
       (with-handlers ([exn:fail? (λ (e) #f)])
         (define-values (p out in err) (subprocess #f #f #f exe "-I" "-S" "-c" worker-source "extract" "<batch>"))
         (define payload (for/list ([it items]) (hasheq 'path (car it) 'text (cadr it) 'entry_lines (caddr it))))
         (write-string (jsexpr->string payload) in)
         (close-output-port in)
         (define out-text (box ""))
         (define t1 (thread (λ () (set-box! out-text (port->string out)))))
         (define t2 (thread (λ () (port->string err))))
         (define done? (sync/timeout timeout p))
         (unless done? (subprocess-kill p #t))
         (subprocess-wait p)
         (thread-wait t1) (thread-wait t2)
         (close-input-port out) (close-input-port err)
         (and done? (eqv? (subprocess-status p) 0)
              (let ([in2 (open-input-string (unbox out-text))])
                (let loop ([acc '()])
                  (define v (with-handlers ([exn:fail? (λ (e) eof)]) (read-json in2)))
                  (if (eof-object? v) (reverse acc) (loop (cons v acc)))))))))

;; items: (listof (cons path text)) → (listof file-facts), same order. A file that fails to parse gets
;; an empty (but present) file-facts, never dropped; python3 missing does the same for every item.
(define (py-extract-batch items)
  (cond
    [(null? items) '()]
    [(not (python-available?)) (for/list ([it items]) (file-facts (car it) 'python '() '() '() #f ""))]
    [else
     (define entries (for/list ([it items]) (list (car it) (cdr it) (python-entry-lines (cdr it)))))
     (define results (run-python-worker-extract entries))
     (cond
       [(and results (= (length results) (length items))) (map jsexpr->file-facts results)]
       [else (for/list ([it items]) (file-facts (car it) 'python '() '() '() #f ""))])]))

;; the lang.rkt gate's `extract` slot: (text file) -> file-facts, same six-function contract
;; rkt-extract/cs-extract implement.
(define (py-extract text path) (car (py-extract-batch (list (cons path text)))))

;; resolve-import: a dotted spec resolves under the importing file's own package (relative imports,
;; leading dots) or under the project root / a top-level `src/` layout (absolute imports) - a file or
;; a package's __init__.py. No dotted spec that fails to match a real project file is ever forced to
;; match one: it is external (a `uses` fact / an external graph edge), same as an unresolvable Racket
;; require.
(define (py-resolve-import lang spec importing-path root all-paths)
  (define (rel->fwd p) (string-replace (path->string p) "\\" "/"))
  (define (path-of dir rel-slashes suffix)
    (with-handlers ([exn:fail? (λ (e) #f)])
      (rel->fwd (find-relative-path (simplify-path (build-path root)) (simplify-path (build-path dir (string-append rel-slashes suffix)))))))
  (define (candidates-under dir rel-slashes)
    (filter (λ (p) (member p all-paths))
            (filter values (list (and (non-empty-string? rel-slashes) (path-of dir rel-slashes ".py"))
                                  (path-of dir (if (non-empty-string? rel-slashes) (string-append rel-slashes "/__init__") "__init__") ".py")))))
  (cond
    [(not (string? spec)) '()]
    [(equal? spec "") '()]
    [(char=? (string-ref spec 0) #\.)
     (define m (regexp-match #px"^([.]+)(.*)$" spec))
     (define dots (string-length (cadr m)))
     (define rest (string-replace (caddr m) "." "/"))
     (define base-dir
       (let loop ([d (let-values ([(d _n _x) (split-path (build-path root importing-path))]) d)] [n (sub1 dots)])
         (if (<= n 0) d (let-values ([(d2 _n2 _x2) (split-path d)]) (loop d2 (sub1 n))))))
     (candidates-under base-dir rest)]
    [else
     (define rel (string-replace spec "." "/"))
     (define c1 (candidates-under root rel))
     (if (pair? c1) c1 (candidates-under (build-path root "src") rel))]))
