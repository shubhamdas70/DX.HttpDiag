# DX.HttpDiag

[![Delphi](https://img.shields.io/badge/Delphi-12.2+-red.svg)](https://www.embarcadero.com/products/delphi)
[![Platform](https://img.shields.io/badge/Platform-Windows-blue.svg)](https://www.microsoft.com/windows)
[![License](https://img.shields.io/badge/License-MIT-green.svg)](LICENSE)
[![GitHub release](https://img.shields.io/github/v/release/omonien/DX.HttpDiag)](https://github.com/omonien/DX.HttpDiag/releases)

**A comprehensive HTTPS URL diagnostic command-line tool for Windows.**

DX.HttpDiag helps you diagnose HTTPS connectivity issues by providing detailed information about:

- 🕐 System time and timezone configuration
- 🌐 Proxy settings (environment variables and WinHTTP/IE)
- 📡 DNS resolution
- 🔒 TLS/SSL certificate validation and analysis
- 📋 HTTP response headers

## Features

- **Certificate Analysis**: Detailed certificate information including validity period, issuer, subject, signature algorithm, key size, and protocol
- **Certificate Warnings**: Automatic detection of expired certificates, self-signed certificates, weak algorithms (SHA-1, MD5), and weak key sizes
- **ECC Support**: Proper handling of Elliptic Curve Cryptography (ECC) certificates
- **Insecure Mode**: Option to bypass certificate validation for diagnostic purposes
- **HEAD/GET Fallback**: Automatically falls back to GET with Range header if HEAD is not supported
- **Colored Output**: Easy-to-read color-coded console output

## Installation

### Pre-built Binary

Download the latest release from the [Releases](https://github.com/omonien/DX.HttpDiag/releases) page.

### Build from Source

Requirements:
- Delphi 12.2 or later (RAD Studio 12.2 Athens or later)
- Windows 10/11

```powershell
# Clone the repository
git clone https://github.com/omonien/DX.HttpDiag.git
cd DX.HttpDiag

# Build using the included build script
powershell -ExecutionPolicy Bypass -File "Build\DelphiBuildDPROJ.ps1" -ProjectFile "Source\DX.HttpDiag.dproj" -Config Release -Platform Win64
```

The compiled executable will be in `Source\Win64\Release\DX.HttpDiag.exe`.

## Usage

```
DX.HttpDiag.exe <URL> [options]
```

### Options

| Option | Description |
|--------|-------------|
| `--timeout=<ms>` | Request timeout in milliseconds (default: 30000) |
| `--insecure` | Accept invalid certificates (for diagnostics only!) |
| `--help`, `-h`, `-?` | Show help message |
| `--version` | Show version information |

### Examples

```powershell
# Basic usage
DX.HttpDiag.exe https://www.example.com

# With custom timeout (15 seconds)
DX.HttpDiag.exe https://www.example.com --timeout=15000

# Diagnose a site with certificate issues (bypass validation)
DX.HttpDiag.exe https://expired.badssl.com --insecure
```

## Sample Output

```
DX.HttpDiag - HTTPS URL Diagnostic Tool
========================================

Target URL: https://www.google.com
Timeout: 30000 ms

=== System Time ===
Local Time: 2026-01-17 18:00:00
UTC Time: 2026-01-17 17:00:00
UTC Offset: -60 minutes (-1 hours)

=== Proxy Configuration (Environment) ===
  No proxy environment variables set

=== DNS Resolution ===
Hostname: www.google.com
  IPv4 [1]: 216.58.206.68

=== HTTP/HTTPS Diagnostic ===
SUCCESS: Request completed successfully

Status Code: 200 OK
Request Duration: 450 ms

=== Certificate Details ===
Certificate Name: www.google.com
Subject: www.google.com
Issuer: US;Google Trust Services;WE2

Validity Period:
  Not Before: 2025-12-29 19:53:15
  Not After: 2026-03-23 19:53:14
  Days Remaining: 65

Cryptographic Information:
  Signature Algorithm: 1.2.840.10045.4.3.2
  Key Size: 256 bits
  Protocol: TLS1_2

SUCCESS: No certificate issues detected

=== Diagnostic Complete ===
```

## Use Cases

- **Troubleshooting TLS/SSL issues**: Identify expired or invalid certificates
- **Proxy debugging**: Verify proxy configuration in corporate environments
- **DNS verification**: Confirm hostname resolution
- **Certificate monitoring**: Check certificate expiration dates
- **Security audits**: Detect weak cryptographic algorithms or key sizes

## Technical Details

- Uses native Windows HTTP stack (`System.Net.HttpClient` / WinHTTP)
- No external dependencies (no OpenSSL, no Indy)
- Single executable, no installation required
- Windows-focused with full WinHTTP proxy support

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Author

**Olaf Monien** - [GitHub](https://github.com/omonien)

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

