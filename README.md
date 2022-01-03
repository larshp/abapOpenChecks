abapOpenChecks
==============

Open source checks for SAP Code Inspector / ABAP Test Cockpit (SCI/ATC)

* For running Continuous Integration outside the ABAP platform see https://abaplint.org
* For Steampunk installations see https://github.com/abapOpenChecks/abapOpenChecks-Steampunk

**Installation**
- install via [abapGit](https://abapgit.org) *(make sure you are using the latest version)* using the online or offline option
- activate checks in SCI (depending on release)
  - Goto -> Management of -> Tests
  - Code Inspector -> Management of -> Tests
  - Code Inspector -> Management of -> Checks 
- configure check variant
- run inspection

**Updating**
- Recommend backing up variants before updating abapOpenChecks, use [upDOWNci](https://github.com/larshp/upDOWNci)
- Pull latest changes via abapGit

**Design goals**
- try and see if it works out
- performance not high priority
- unit testable without creating new objects in customer system
- only english supported

Version requirement: v740sp02 or higher

## [Documentation](https://docs.abapopenchecks.org)
