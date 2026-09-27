#lang racket/base
;; Python support (milestone XL1): facts come from Python's own stdlib `ast`/`compile`, run by a worker
;; script in the installed python3. Nothing about Python's grammar is re-implemented here, and nothing
;; is imported or executed: the worker only parses and compiles (compiling also finds semantic errors
;; such as `return` outside a function). Same pattern as api.rkt: an embedded script, a subprocess,
;; a timeout, a clear "skipped" when python3 is missing.
;;   syntax gate (T45): a SyntaxError becomes a located finding; for the shapes Python names precisely
;;   (a bracket never closed, an unmatched or mismatched closer, a missing colon) the worker also tries an
;;   edit and offers it only when the file compiles afterwards ("verified").
(require racket/list racket/string racket/port json
         "common.rkt" "syntax-check.rkt")
(provide python-gate-check python-available? run-python-worker python-find-anchor python-list-names)

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
