# APAC Accounting and Business Management

Mirror of an accounting software in COBOL from [sourceforge](https://sourceforge.net/projects/apac-accounting/), written by James William Lemmon released under GNU GPL v2 (`New Development` is released under GNU GPL v3).

Accounting Including: Point-of-sale, Invoicing, Job Costing, Production, Orders, Quotations, Inventory (Stock), Accounts Receivable (Debtors), Accounts Payable (Creditors), General Ledger, Management Reports, Reconciliations.

Require Micro Focus COBOL or GnuCOBOL to compile.

Manual (not complete) is in `DOC` directory.

# Original readme

The latest source is for Ver 8.15 (Apacsrc15.exe).

Version 8.14 source has not been deleted (Apacsrc8.zip).

The Manual that is currently available (not complete - BUSY WRITING A NEW ONE) (ApacManual-7.zip)

The source, that is being developed is available in trunk/New Development (which also contains convertion programs from the pervious data files to the new layouts).

This version compiles and runs with Micro Focus COBOL. - A version for GnuCobol will be ready once I have resolved the split key indexing. As MF Cobol requires licensing, there is no binary version on this site as the runtimes are required as well.

The print routines used in some of the programs need at least MF Object Cobol as it has the PRN calls available. If Windows controlled USB printers are not needed then this does not apply as all reports are available on LPT and COM printers without these routines.

The USB printers allow for Business LOGO's to be printed.

If anyone needs some Demo data, please let me know and I will upload it onto the site.

The system requires registration to run a full business and as it is at present will run in demo mode if not registered.

I am busy removing the registration code and if anyone wants to use the system, not in demo mode then the override password when initializing a new set of books is 918273. To add workstations (only four additional at a time) the password is 112233.

I am currently analising and doing the necessary to get the system to run on GnuCOBOL.

The idea is to, after getting it running on GnuCOBOL to then make the changes to have a 100% GUI interface.

The manual is for version 7 of the system and this I will get up to date, once I have the system running on GnuCobol.

The idea is to have it runing on MF with a GUI interface as well. This will first be done for Windows and then for Linux, which hopefully will not be too difficult.

As I am the only person involved it would be nice if any of you would like to join the project as developers to assist.
