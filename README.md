# PascalTextEditor  

PascalTextEditor is a lightweight terminal-based text editor written in **Pascal**, designed to provide **basic syntax highlighting** for Markdown, Python, and Pascal.  
It is ideal for **lightweight coding, quick note-taking, and Pascal programming**, especially for users who prefer a **minimalist environment**.  

## **Features**  
✔ Syntax highlighting for **Markdown (`.md`)**, **Python (`.py`)**, and **Pascal (`.pas`)**.  
✔ Automatic syntax detection based on **file extension**.  
✔ **Failsafe syntax detection** (if syntax files are missing, highlighting is disabled).  
✔ Support for **Pascal-style comments** (`(* ... *)`) and **Python comments** (`#`).  
✔ Lightweight, **fast**, and **minimal dependencies**.  

---

## **Theoretical Use Cases**  

### **1. Quick Coding Sessions**  
If you need to **edit or review** Python or Pascal code quickly from a terminal without launching a heavy IDE, this editor provides **color-coded syntax** to improve readability.  

### **2. Markdown Note-Taking**  
Write Markdown-formatted notes with **headers, bold, italics, and code blocks** clearly highlighted.  

### **3. Learning Pascal**  
Since the editor itself is **written in Pascal**, it can be used as a learning tool for **Pascal programming**, with built-in syntax highlighting for the language.  

### **4. Embedded Systems & Low-Resource Devices**  
Running on a **lightweight Pascal compiler**, this editor is perfect for **low-resource environments** such as **Raspberry Pi**, **old computers**, or **embedded systems** where performance matters.  

---

## **Installation & Compilation**  

### **Dependencies**  
You'll need **Free Pascal Compiler (FPC)** installed.  

#### **Debian/Ubuntu:**  
```bash
sudo apt install fpc

#### **arch/manjaro/endeavourOS:**
```bash 
sudo pacman -S fpc

#### **alpine linux:**
```bash
sudo apk add fpc

#### **gentoo:**
```bash
sudo emerge dev-lang/fpc




