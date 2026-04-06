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
│   ├── CONNMGMT.CPY            # Connection management
│   ├── VIEWNET.CPY             # View network
│   ├── JOBS_SRC.CPY            # Week 6 job posting
│   ├── BROWSEJOBS_SRC.CPY      # Week 7 browse jobs
│   ├── APPLYJOB_SRC.CPY        # Week 7 apply jobs
│   ├── VIEWAPPS_SRC.CPY        # Week 7 application report
│   ├── SENDMESSAGE_SRC.CPY     # Week 8 send messages
│   ├── VIEWMESSAGES_SRC.CPY    # Week 8 message view placeholder
│   ├── accounts_info.dat       # User accounts and profile data
│   ├── pending_requests.dat    # Pending connection requests
│   ├── connections.dat         # Accepted user connections
│   ├── jobs.dat                # Job and internship postings
│   ├── applications.dat        # Job applications
│   └── messages.dat            # Private messages
├── Roles.txt                   # Team roles 
├── Epic1-Storyx-Test-Input     # Sample test inputs for Week 1
├── Epic1-Storyx-Test-Output    # Sample test outputs for Week 1
├── Epic2-Storyx-Test-Input     # Sample test inputs for Week 2
├── Epic2-Storyx-Test-Output    # Sample test outputs for Week 2
├── Epic3-Storyx-Test-Input     # Sample test inputs for Week 3
├── Epic3-Storyx-Test-Output    # Sample test outputs for Week 3
├── Epic4-Storyx-Test-Input     # Sample test inputs for Week 4
├── Epic4-Storyx-Test-Output    # Sample test outputs for Week 4
├── Epic5-Storyx-Test-Input     # Sample test inputs for Week 5
├── Epic5-Storyx-Test-Output    # Sample test outputs for Week 5
├── Epic6-Storyx-Test-Input     # Sample test inputs for Week 6
├── Epic6-Storyx-Test-Output    # Sample test outputs for Week 6
├── Epic7-Storyx-Test-Input     # Sample test inputs for Week 7
├── Epic7-Storyx-Test-Output    # Sample test outputs for Week 7
├── Epic8-Storyx-Test-Input     # Sample test inputs for Week 8
├── Epic8-Storyx-Test-Output    # Sample test outputs for Week 8
├── accounts_info.dat           # Stored account information for Week 1, 2, 3, 4, 5, 6, 7, 8 test cases
├── InCollege-Input.txt         # Example input for Week 8
├── InCollege-Output.txt        # Example output for Week 8
└── README.md                   # This file
```

## Tech Stack
- COBOL
- Docker

## Running the program
**1. Open the project in Docker**

Ensure the project is opened inside the Docker container and you are in the project root directory.

The expected structure is:

-Source code, copybooks, and persistent data files are stored in `src/`

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
- Week 5: Accept/reject connection requests, view network of connections, prevent duplicate requests, persistent network updates
- Week 6: Post job and internship listings with persistent storage
- Week 7: Browse jobs, view full details, apply to jobs
- Week 8: Private messaging system (send messages to connected users)

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
- Users can view all pending connection requests and choose to accept or reject each request individually.
- Multiple pending requests handled correctly in sequence, with confirmations displayed for each action.
- Rejected requests are removed from the pending list and do not appear in the network.
- Accepted requests are added to the user’s network and stored persistently in connections.dat.
- Users cannot send duplicate connection requests to existing connections.
- Network viewing displays all current connections with details including:
  - Name
  - University
  - Major
- Empty network and empty pending requests are handled gracefully, displaying appropriate messages.
- Menu navigation allows returning to the main menu after request processing or network viewing.
- All updates are reflected consistently across multiple logins, ensuring data persistence.
- File-based input/output continues to ensure console display and output file are identical.


### Week 6 – Job Posting System
**Tasks Completed:**
- Added a functional Job Search/Internship menu
- Implemented a Post a Job/Internship option
- Captured required fields:
  - Job Title
  - Description
  - Employer
  - Location
- Added an optional Salary field
- Stored all job postings persistently in `jobs.dat`
- Added support for returning to the main menu
- Continued file-based input/output consistency
- All prompts and confirmations displayed on screen and written identically to output file
- Added modular job posting copybook support


### Week 7 – Browse Jobs & Applications
**Tasks Completed:**
- Implemented the Browse Jobs/Internships
- Displayed all available job listings with:
  - Job Title
  - Employer
  - Location
- Added ability to select a job and view full details
- Added the 'Apply for this Job' functionality
- Persistently stored applications in `applications.dat`
- Added confirmation messages after successful application
- Added a View My Applications option
- Report includes:
  - Job Title
  - Employer
  - Location
  - Total application count
- Continued identical console and output-file logging
- Added modular copybooks for applying and report generation


### Week 8 – Messaging System (Part 1)
**Tasks Completed:**
- Added Messages option to post-login main menu
- Added 'Send a New Message' option
- Added placeholder for 'View My Messages' option
- Implemented recipient username input
- Added validation to ensure recipient is an accepted connection
- Prevented messaging non-connections or pending users
- Prevented an empty message from being sent
- Added free-form message content input, with maximum 200 chars message only allowed
- Persistently stored messages in `messages.dat`
- Stored:
  - Sender username
  - Recipient username
  - Message content
  - Timestamp
- Displayed confirmation after successful send
- Continued file-based input/output consistency
- Added modular copybooks for messaging support
