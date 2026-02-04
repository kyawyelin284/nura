#!/usr/bin/env sh
set -eu

REPO="kyawyelin284/nura"
VERSION="v1.0.1"
TARGET_DIR="${HOME}/bin"

os="$(uname -s | tr '[:upper:]' '[:lower:]')"
case "$os" in
  linux*) asset="nura-linux" ;;
  darwin*) asset="nura-macos" ;;
  msys*|mingw*|cygwin*) asset="nura-win.exe" ;;
  *) echo "Unsupported OS: $os" >&2; exit 1 ;;
esac

mkdir -p "$TARGET_DIR"
cache_bust="$(date +%s)"
url="https://github.com/${REPO}/releases/download/${VERSION}/${asset}?cachebust=${cache_bust}"
dest="${TARGET_DIR}/nura"

echo "Downloading ${url}..."
curl -fsSL "$url" -o "$dest"
chmod +x "$dest"

if ! command -v nura >/dev/null 2>&1; then
  export PATH="${TARGET_DIR}:${PATH}"
fi

echo "Installed to ${dest}"
echo "Verifying..."
"${dest}" --version
