# InCollege Project (COBOL)

InCollege is a university-focused professional networking system implemented in COBOL.
This repository contains the core functionality for user login, account creation,
input/output formats, and associated test cases.

Project Structure

```
├── bin/                        # Compiled executable files
├── src/
│   ├── InCollege.cob           # Source COBOL code
│   ├── SENDREQ.CPY             # Source COBOL code for sending connection requests
│   ├── VIEWREQ.CPY             # COBOL code fore viewing connection requests
├── Roles.txt                   # Team roles
├── Epic1-Storyx-Test-Input     # Sample test inputs for Week 1
├── Epic1-Storyx-Test-Output    # Sample test outputs for Week 1
├── Epic2-Storyx-Test-Input     # Sample test inputs for Week 2
├── Epic2-Storyx-Test-Output    # Sample test outputs for Week 2
├── Epic3-Storyx-Test-Input     # Sample test inputs for Week 3
├── Epic3-Storyx-Test-Output    # Sample test outputs for Week 3
├── Epic4-Storyx-Test-Input     # Sample test inputs for Week 4
├── Epic4-Storyx-Test-Output    # Sample test outputs for Week 4
├── accounts_info.dat           # Stored account information for Week 1,2, 3, 4 test cases
├── InCollege-Input.txt         # Example input for Week 4
├── InCollege-Output.txt        # Example output for Week 4
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
- Week 3: Enhanced profile viewing (complete profile display), basic user search by full name, exact match search results, dual I/O consistency for profile viewing and search functionality
- Week 4: Send connection requests, store pending requests, view received connection requests

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

### Week 3 – Profile Viewing & Basic Search
**Tasks Completed:**
- Enhanced "View My Profile" option to display complete saved profile information
- Display of all required fields: First Name, Last Name, University/College, Major, Graduation Year
- Display of optional sections: About Me, Experience, and Education entries
- Clear and readable formatting of profile output in the console
- Implemented functional "Find someone you know" feature
- Exact match search by full name (First Name + Last Name)
- Sequential scan of stored user account/profile data to locate matches
- Display of full profile information when a matching user is found
- Message shown when no matching user exists
- Continued file-based input handling for menu selections and search queries
- All output displayed on screen and written identically to the output file
- Integrated search feature into the post-login menu flow
- Program returns to main menu after profile viewing or search operations

### Week 4 – Connection Requests
**Tasks Completed:**
- Implemented option to send a connection request after viewing another user's profile
- Display confirmation message when a request is sent
- Stored pending connection requests persistently using a file
- Added menu option to view received connection requests
- Display list of users who have sent pending requests
- Show message when no pending requests exist
- Basic validation to prevent duplicate or invalid requests
- Continued file-based input handling for request actions
- All output displayed on screen and written identically to the output file


### Week 5 – Connection Requests
**Tasks Completed:**
