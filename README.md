# InCollege Project (COBOL)

InCollege is a university-focused professional networking system implemented in COBOL. 
This repository contains the core functionality for user login, account creation,
input/output formats, and associated test cases.

Project Structure

```
├── bin/                        # Compiled executable files
├── src/                    
│   ├── InCollege.cob           # Source COBOL code
├── Roles.txt                   # Team roles
├── Epic1-Storyx-Test-Input     # Sample test inputs for Week 1
├── Epic1-Storyx-Test-Output    # Sample test outputs for Week 1
├── Epic2-Storyx-Test-Input     # Sample test inputs for Week 2
├── Epic2-Storyx-Test-Output    # Sample test outputs for Week 2
├── Epic3-Storyx-Test-Input     # Sample test inputs for Week 3
├── Epic3-Storyx-Test-Output    # Sample test outputs for Week 3
├── accounts_info.dat           # Stored account information for Week 1,2 and 3 test cases
├── InCollege-Input.txt         # Example input for Week 3
├── InCollege-Output.txt        # Example output for Week 3
└── README.md                   # This file
```

## Tech Stack
- COBOL
- Docker

## Running the program
**1. Open the project in Docker**

Ensure the project is opened inside the Docker container and you are in the project root directory.

The expected structure is:

-Source code in src/

-Executable in bin/

-Input and output files in the project root


**2. Compile the COBOL program**


Make sure you are in the src folder and have opened the InCollege.cob file.
``` Press 'Ctrl+Shift+B' ``` to compile it.


**3. Run using the default input and output files**


Running the program without arguments uses the default files:

-Input file: InCollege-Input.txt

-Output file: InCollege-Output.txt

In terminal enter ```./bin/InCollege```

## Features implemented
- Week 1: User login, account creation, input/output handling
- Week 2: Profile creation and editing, optional sections (About Me, Experience, Education), profile viewing

## Weekly Epics

### Week 1 - User Login and Registration
**Tasks Completed:**
- User account creation with password validation
- Login validation for existing users
- User input parsing from input file
- Dual I/O integration (screen output and output file to match)
- Storing account information in a file

### Week 2 – User Profile Creation and Management
**Tasks Completed:**
- Profile creation and editing after successful login
- Capture and validation of required profile information
- Optional profile sections (About Me, Experience, Education)
- Persistent storage of profile data linked to user accounts
- Profile viewing functionality
- Continued file-based input and output handling

