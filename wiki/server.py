#!/usr/bin/env python3
"""WAL-FAST wiki — local server. Run: python3 wiki/server.py"""
import http.server, json, os, socketserver
from pathlib import Path

PORT = 7878
PAGES_DIR = Path(__file__).parent / "pages"

class WikiHandler(http.server.SimpleHTTPRequestHandler):
    def __init__(self, *a, **kw):
        super().__init__(*a, directory=str(Path(__file__).parent), **kw)

    def do_GET(self):
        if self.path == "/api/pages":
            pages = sorted([
                {"slug": f.stem, "title": f.stem.replace("-", " ").title()}
                for f in PAGES_DIR.glob("*.md")
            ], key=lambda x: x["slug"])
            self._json(pages)
        elif self.path.startswith("/api/page/"):
            slug = self.path[len("/api/page/"):]
            path = PAGES_DIR / f"{slug}.md"
            if path.exists():
                self._text(path.read_text())
            else:
                self.send_error(404)
        else:
            super().do_GET()

    def _json(self, data):
        body = json.dumps(data).encode()
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", len(body))
        self.end_headers()
        self.wfile.write(body)

    def _text(self, text):
        body = text.encode()
        self.send_response(200)
        self.send_header("Content-Type", "text/plain; charset=utf-8")
        self.send_header("Content-Length", len(body))
        self.end_headers()
        self.wfile.write(body)

    def log_message(self, fmt, *args):
        pass  # silent

with socketserver.TCPServer(("", PORT), WikiHandler) as httpd:
    print(f"WAL-FAST wiki → http://localhost:{PORT}")
    httpd.serve_forever()
