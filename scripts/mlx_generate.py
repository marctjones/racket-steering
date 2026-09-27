#!/usr/bin/env python3
"""Generate completions for prompts.jsonl with a local MLX model (Apple Silicon).

  .venv-mlx/bin/python scripts/mlx_generate.py PROMPTS.jsonl OUT.jsonl [--model ID] [--max-tokens N]
                                              [--thinking] [--limit N]

Input lines:  {"id": ..., "prompt": "<user message>"}
Output lines: {"id", "completion", "prompt_tokens", "completion_tokens", "seconds"}
Resumable: ids already in OUT are skipped. Greedy decoding (temperature 0), so a rerun is reproducible up
to floating-point nondeterminism. Writes OUT.meta.json with the model and settings. Offline by default:
the model must already be in the Hugging Face cache (HF_HUB_OFFLINE=1); pass --online to allow a download.
"""
import argparse, json, os, sys, time


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("prompts")
    ap.add_argument("out")
    ap.add_argument("--model", default="mlx-community/Qwen3.5-9B-MLX-4bit")
    ap.add_argument("--max-tokens", type=int, default=1500)
    ap.add_argument("--thinking", action="store_true", help="enable the model's reasoning mode (long outputs)")
    ap.add_argument("--limit", type=int, default=0)
    ap.add_argument("--online", action="store_true")
    a = ap.parse_args()
    if not a.online:
        os.environ["HF_HUB_OFFLINE"] = "1"

    from mlx_lm import load, generate
    from mlx_lm.sample_utils import make_sampler

    done = set()
    if os.path.exists(a.out):
        with open(a.out) as f:
            for line in f:
                try:
                    done.add(json.loads(line)["id"])
                except Exception:
                    pass
    with open(a.prompts) as f:
        items = [json.loads(l) for l in f if l.strip()]
    todo = [i for i in items if i["id"] not in done]
    if a.limit:
        todo = todo[: a.limit]

    t0 = time.time()
    model, tok = load(a.model)
    with open(a.out + ".meta.json", "w") as f:
        json.dump({"model": a.model, "max_tokens": a.max_tokens, "thinking": a.thinking, "temperature": 0.0,
                   "mlx_lm": __import__("mlx_lm").__version__, "load_seconds": round(time.time() - t0, 1)}, f, indent=1)
    sampler = make_sampler(temp=0.0)
    print(f"{len(done)} done, {len(todo)} to do; model loaded in {time.time() - t0:.1f}s", flush=True)

    with open(a.out, "a") as out:
        for n, item in enumerate(todo, 1):
            msgs = [{"role": "user", "content": item["prompt"]}]
            try:
                prompt = tok.apply_chat_template(msgs, add_generation_prompt=True, tokenize=False,
                                                 enable_thinking=a.thinking)
            except TypeError:
                prompt = tok.apply_chat_template(msgs, add_generation_prompt=True, tokenize=False)
            t = time.time()
            try:
                text = generate(model, tok, prompt=prompt, max_tokens=a.max_tokens, sampler=sampler, verbose=False)
                err = None
            except Exception as e:  # keep going; the scorer sees an empty completion
                text, err = "", repr(e)
            dt = time.time() - t
            rec = {"id": item["id"], "completion": text, "prompt_tokens": len(tok.encode(prompt)),
                   "completion_tokens": len(tok.encode(text)) if text else 0, "seconds": round(dt, 2)}
            if err:
                rec["error"] = err
            out.write(json.dumps(rec) + "\n")
            out.flush()
            if n % 5 == 0 or n == len(todo):
                print(f"[{n}/{len(todo)}] {item['id']} {rec['completion_tokens']} tok {dt:.0f}s", flush=True)


if __name__ == "__main__":
    sys.exit(main())
