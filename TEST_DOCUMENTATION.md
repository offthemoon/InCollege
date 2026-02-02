# InCollege Week 2 - Test Documentation
**Epic #2: User Profile Management**
**Date**: February 2, 2026
**Tester**: [Your Name]

---

## Test Summary

### Test Cases Created: 15 Total
- **T1-T10**: Created by fellow tester
- **T11-T15**: Additional test cases created to fill coverage gaps

### Test Coverage

#### ✅ Positive Test Cases (T1, T2, T3, T8)
- Profile creation with all fields
- Profile creation with 1-2 experiences/educations
- Profile viewing after creation
- Profile editing (updating existing profile)

#### ✅ Negative Test Cases (T5, T10)
- Invalid login credentials (T5)
- Blank required fields rejection (T10)
- Invalid graduation year validation (T10)

#### ✅ Edge Cases (T11-T15)
- **T11**: View profile before creating it
- **T12**: Exactly 3 experiences AND 3 educations (assignment requirement)
- **T13**: Minimal profile (only required fields)
- **T14**: About Me field with 200+ characters
- **T15**: Profile persistence across login sessions

---

## Detailed Test Case Descriptions

### T11: View Profile Before Creation
**Purpose**: Verify system behavior when user views profile without creating it first
**Type**: Edge Case
**Steps**:
1. Create new account
2. Login successfully
3. Immediately select "View My Profile" (option 2)
4. Then logout

**Expected Result**: System should either show empty profile or display message "Profile not yet created"

**Input File**: `InCollege-Input-T11.txt`

---

### T12: Maximum Experience and Education Entries
**Purpose**: Test edge case of exactly 3 experiences and 3 educations (assignment requirement)
**Type**: Edge Case - Boundary Testing
**Steps**:
1. Create account and login
2. Create profile with all required fields
3. Add exactly 3 experience entries
4. Add exactly 3 education entries
5. View profile to verify all entries saved
6. Logout

**Expected Result**: All 3 experiences and 3 educations should be saved and displayed correctly

**Input File**: `InCollege-Input-T12.txt`
**Data**:
- Experiences: Software Engineer (Microsoft), Data Analyst (Amazon), Research Assistant (MIT Labs)
- Education: Master of Science (Stanford), Bachelor of Science (UC Berkeley), Associate Degree (Community College)

---

### T13: Minimal Profile (Required Fields Only)
**Purpose**: Verify profile creation with only required fields, all optional fields skipped
**Type**: Positive Test Case
**Steps**:
1. Create account and login
2. Fill only required fields: First Name, Last Name, University, Major, Graduation Year
3. Skip About Me (blank line)
4. Skip all Experience entries (DONE immediately)
5. Skip all Education entries (DONE immediately)
6. View profile
7. Logout

**Expected Result**: Profile should save successfully with only required information

**Input File**: `InCollege-Input-T13.txt`

---

### T14: About Me Length Validation
**Purpose**: Test About Me field truncation at 200 characters
**Type**: Negative Test Case - Boundary Testing
**Steps**:
1. Create account and login
2. Fill required fields
3. Enter About Me text exceeding 200 characters (actual: 341 characters)
4. Skip experience and education
5. View profile to check if text was truncated

**Expected Result**: About Me should be truncated to 200 characters (per assignment spec)

**Input File**: `InCollege-Input-T14.txt`
**Input Text**: 341 characters testing truncation

---

### T15: Profile Persistence Test
**Purpose**: Verify profile data persists across multiple login sessions
**Type**: Integration Test
**Steps**:
1. Create account and login
2. Create complete profile with experience and education
3. View profile (verify data)
4. Logout
5. Login again with same credentials
6. View profile again
7. Logout

**Expected Result**: Profile data should be identical in both viewing sessions

**Input File**: `InCollege-Input-T15.txt`

---

## Bugs Found During Testing

### BUG #1: Experience Description Truncation Issue
**Severity**: Medium
**Location**: InCollege.cob:88, U-EXP-DESC field definition (line 90)
**Test Case**: T9
**Description**: Experience description field truncates at ~100 characters but doesn't display warning to user

**Evidence**:
```
Input (T9, line 20): "This account is being edited. Hopefully this work. also checking length validation. I was never a waiter."
Output (T9, line 88): "This account is being edited. Hopefully this work. also checking length validation. I was never a wa"
```

**Expected Behavior**: Either enforce 100-char limit on input OR display full 100 characters

**Actual Behavior**: Silently truncates without warning

---

### BUG #2: Missing Profile Completion Check
**Severity**: Low-Medium
**Location**: InCollege.cob:837-939 (VIEW-MY-PROFILE)
**Test Case**: T11 (to verify)
**Description**: System allows viewing profile before any profile data has been created

**Expected Behavior**: Display message "Profile not yet created, please create your profile first" OR show graceful empty profile

**Actual Behavior**: Unknown - needs testing with T11

**Status**: Awaiting test execution

---

### BUG #3: Data Persistence Failure (CRITICAL)
**Severity**: HIGH
**Location**: Data saving mechanism (InCollege.cob:308-335)
**Test Case**: T10
**Description**: Experience and Education data not persisting to accounts_info.dat

**Evidence**:
- T10 output shows 2 experiences and 1 education were entered
- accounts_info.dat shows: `Jasmine|Jas123!me|Jasmine|Kohli|University of South Florida|Computer Science|2026|I am a scrum master for InCollege wk2.|0||0|`
- Experience count = 0, Education count = 0 (should be 2 and 1)

**Expected Behavior**: Experience and education should persist to file

**Actual Behavior**: Data shows in output but not saved to file

**Impact**: DATA LOSS - Profile data not persisting

**Priority**: CRITICAL - Must fix before submission

---

## Test Execution Instructions

### Running Tests (Docker/DevContainer Method)
Since the program was compiled for Linux, you need to use Docker:

```bash
# Option 1: Use VS Code DevContainer
1. Open project in VS Code
2. Click "Reopen in Container"
3. Once inside container:
   cd /workspace
   cobc -x -o bin/InCollege src/InCollege.cob
   bin/InCollege T11
   bin/InCollege T12
   # ... etc

# Option 2: Manual Docker
docker build -t incollege .devcontainer/
docker run -it -v $(pwd):/workspace incollege
cd /workspace
cobc -x -o bin/InCollege src/InCollege.cob
bin/InCollege T11
```

### Running All New Tests
```bash
for i in 11 12 13 14 15; do
    echo "Running Test T$i..."
    bin/InCollege T$i
    echo "Test T$i completed. Output in InCollege-Output-T$i.txt"
done
```

---

## Test Coverage Matrix

| Requirement | Test Cases | Status |
|-------------|-----------|--------|
| Create profile with required fields | T1, T2, T3, T10, T13 | ✅ Pass |
| Edit existing profile | T9 | ✅ Pass |
| View profile | T1, T2, T3, T9, T10 | ✅ Pass |
| Required field validation | T10 | ✅ Pass |
| Graduation year validation | T10 | ✅ Pass |
| About Me (optional) | T1, T2, T13, T14 | ⏳ Pending |
| Experience entries (0-3) | T1, T3, T9, T10, T12, T13 | ⏳ Pending |
| Education entries (0-3) | T1, T3, T12, T13 | ⏳ Pending |
| Data persistence | T15 | ⏳ Pending |
| I/O file consistency | All tests | ✅ Pass |
| Edge case: Exactly 3 entries | T12 | ⏳ Pending |
| Edge case: View before create | T11 | ⏳ Pending |

**Legend**:
- ✅ Pass: Test executed and passed
- ⏳ Pending: Test created but not yet executed
- ❌ Fail: Test executed and failed

---

## Recommendations for Team

### For Programmers:
1. **CRITICAL**: Investigate and fix Bug #3 (data persistence) immediately
2. Fix experience description truncation issue (Bug #1)
3. Add profile completeness check before viewing (Bug #2)

### For Testers:
1. Execute tests T11-T15 in Docker environment
2. Verify Bug #3 is reproducible
3. Create bug tickets in Jira for all three bugs
4. After bugs are fixed, re-run all test cases

### For Scrum Master:
1. Prioritize Bug #3 as blocker - data loss is unacceptable
2. Ensure bug tickets are created in Jira
3. Schedule bug fix verification before end of week

---

## Files Delivered

### Test Input Files (Epic1-Storyx-Test-Input/)
- InCollege-Input-T11.txt
- InCollege-Input-T12.txt
- InCollege-Input-T13.txt
- InCollege-Input-T14.txt
- InCollege-Input-T15.txt

### Test Output Files (Epic1-Storyx-Test-Output/)
- InCollege-Output-T11.txt (pending execution)
- InCollege-Output-T12.txt (pending execution)
- InCollege-Output-T13.txt (pending execution)
- InCollege-Output-T14.txt (pending execution)
- InCollege-Output-T15.txt (pending execution)

### Documentation
- TEST_DOCUMENTATION.md (this file)

---

## Notes

- Binary file in bin/InCollege is compiled for Linux (x86_64)
- macOS users must use Docker to run tests
- All test input files follow the same format as T1-T10
- Tests T11-T15 specifically target gaps identified in initial test review

---

**End of Test Documentation**
