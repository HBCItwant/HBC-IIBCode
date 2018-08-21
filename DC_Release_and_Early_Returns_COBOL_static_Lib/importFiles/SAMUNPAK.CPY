      * THIS IS THE'UNPACKED' VERSION OF THE "SAMASTER" LAYOUT.
      *
       01  UNPACKED-HEADER-RECORD.
           05 UHR-KEY.
              08  UHR-COMPARE-KEY.
U                 10 UHR-CRDATE      PIC 9(8).
      * DATE IS CCYYMMDD (ORIGINAL RINGING DATE)
      *      CONTAINS A LEADING 0, TRAILING SIGN
                  10 UHR-STORE       PIC 9999.
                  10 UHR-TERMINAL    PIC 9999.
U             08 UHR-TRANNUM         PIC 9999.
              08 UHR-UNIQUE-QUALIFIER PIC XXX.
              08 UHR-RECSEQ          PIC 9999.
           05 UHR-RECTYPE            PIC 99.
           05 UHR-COMPANY-CODE       PIC 99.
U          05 UHR-CUSTOMER           PIC 9(16).
           05 UHR-TYPE               PIC 99.
           05 UHR-QUAL               PIC 99.
           05 UHR-VOID               PIC 9.
U          05 UHR-ORIG-ASSOCIATE     PIC 9(7).
U          05 UHR-TIME               PIC 9(5).
U          05 UHR-EXP-DATE           PIC 9(5).
U          05 UHR-LAYAWAYFEE REDEFINES
              UHR-EXP-DATE           PIC 9(5).
           05 UHR-LAYAWAYFEE-2D REDEFINES UHR-LAYAWAYFEE
U                                    PIC 9(3)V99.
U          05 UHR-LAYAWAYFEE-SIGN    PIC X.
U          05 UHR-PAYDOCNUM          PIC 9(9).
U          05 UHR-LAYAWAYDEP REDEFINES
              UHR-PAYDOCNUM          PIC 9(9).
           05 UHR-LAYAWAYDEP-2D REDEFINES UHR-LAYAWAYDEP
U                                    PIC 9(7)V99.
U          05 UHR-LAYAWAYDEP-SIGN    PIC X.
U          05 UHR-GROSS              PIC 9(9).
U          05 UHR-GROSS-2D REDEFINES UHR-GROSS PIC 9(7)V99.
U          05 UHR-GROSS-SIGN         PIC X.
U          05 UHR-TAX                PIC 9(5).
           05 UHR-TAX-2D REDEFINES UHR-TAX
U                                    PIC 9(3)V99.
U          05 UHR-TAX-SIGN           PIC X.
           05 UHR-SA-MODIFIED        PIC X.
U          05 UHR-VD-TRAN-NO         PIC 9(8).
U          05 UHR-OPEN-DATE REDEFINES UHR-VD-TRAN-NO PIC 9(8).
           05 UHR-DISCOUNT-PC        PIC V99.
U          05 UHR-DISCOUNT-AMOUNT    PIC 9(9).
           05 UHR-DISCOUNT-AMOUNT-2D REDEFINES UHR-DISCOUNT-AMOUNT
U                                    PIC 9(7)V99.
U          05 UHR-DISCOUNT-AMT-SIGN  PIC X.
           05 UHR-AUTHORIZATION-NO-X.
              06 UHR-AUTHORIZATION-NO   PIC 9(6).
           05 UHR-PROCESSED-FLAG     PIC X.
           05 UHR-OFFLINE-FLAG       PIC X.
           05 UHR-ACCOUNT-TYPE       PIC X.
           05 UHR-DISCOUNT-TYPE REDEFINES UHR-ACCOUNT-TYPE PIC X.
           05 UHR-DEFERRED-PLAN      PIC 9(5).
U          05 UHR-MODIFIED-DATE      PIC 9(8).
      *  UHR-DEFERRED-FLAG, UHR-YNKR-QUAL,
      *  UHR-FORCE-BALANCE, AND UHR-DEFERRED-DEPT
      *  ARE USED FOR YOUNKERS ONLY.
      *  UHR-YNKR-QUAL IS REDEFINED AS UHR-PRSN-SEND-TAX-FLAG.
           05 UHR-DEFERRED-FLAG      PIC X.
           05 UHR-YNKR-QUAL          PIC XX.
           05 UHR-PRSN-SEND-TAX-FLAG REDEFINES UHR-YNKR-QUAL PIC 99.
           05 UHR-CPS-QUAL REDEFINES UHR-YNKR-QUAL PIC 99.
           05 UHR-LOYALTY-CLUB-CNT REDEFINES UHR-YNKR-QUAL PIC 99.
U          05 UHR-RINGING-ASSOC      PIC 9(7).
           05 UHR-MODIFIED-USERID    PIC X(8).
           05 UHR-FORCE-BALANCE      PIC X.
           05 UHR-DEFERRED-DEPT      PIC 9(5).
           05 UHR-TAX-PERCENT REDEFINES UHR-DEFERRED-DEPT
U                                    PIC V9(5).
           05 UHR-SWIPED-FLAG        PIC X.
           05 UHR-ORIGINAL-QUAL      PIC XX.
U          05 UHR-ASSOC-DISC-ON-TAX  PIC 9(7)V99.
U          05 UHR-ASSOC-DISC-SIGN    PIC X.
U          05 UHR-YNK-DISCOUNT-PC    PIC V9(6).
           05 UHR-CPS-AUTH REDEFINES UHR-YNK-DISCOUNT-PC
U                                    PIC 9(6).
U          05 UHR-YNK-DISC-SIGN      PIC X.
           05 UHR-TRANS-TYPEN        PIC X(3).
              88  CUSTOMER-TRAN      VALUE IS 'CUS'.
              88  CASHIER-TRAN       VALUE IS 'CSH'.
              88  PAYMENT-ON-ACCT    VALUE IS 'POA'.
              88  TENDER-EXCHANGE     VALUE IS 'TE '.
      *       88  INVENTORY-TRAN     VALUE IS 'INV'.
              88  NO-SALE             VALUE IS 'NS '.
              88  TERMINAL-OPEN       VALUE IS 'TO '.
              88  TERMINAL-CLOSE      VALUE IS 'TC '.
              88  CHANGE-FILL         VALUE IS 'TCF'.
              88  CHANGE-REQUEST      VALUE IS 'TCR'.
      *       88  CHANGE-RECEIVE      VALUE IS 'TCC'.
      ***********TCC IS NOT USED PER BARBARA AND LINDA 07/29/2005
      ***********POS DOES NOT BURN A TRANSACTION NUMBER
              88  TERMINAL-LOAN-REQ   VALUE IS 'TLR'.
              88  TERMINAL-LOAN-FILL  VALUE IS 'TLF'.
              88  TERMINAL-PICK-UP    VALUE IS 'TPU'.
              88  VOID-TRAN           VALUE IS 'VDT'.
              88  PRE-SELL            VALUE IS 'PRE'.
      *       88  EMPLOYEE-CLOCKING   VALUE IS 'EC '.
              88  VALID-TRANS-TYPEN   VALUE IS 'CUS',
                                               'CSH'
                                               'POA'
                                               'TE '
      *                                        'INV'
                                               'NS '
                                               'TP '
                                               'TC '
                                               'TCF'
                                               'TCR'
      *                                        'TCC'
                                               'TLR'
                                               'TLF'
                                               'TPU'
                                               'VDT'
                                               'PRE'.
           05 UHR-TRANS-SOURCE          PIC X(3).
              88  SERVICE-DESK       VALUE IS 'SDS'.
              88  POINT-OF-SALE      VALUE IS 'POS'.
              88  BACK-OFF-RING      VALUE IS 'BOR'.
              88  MANUAL-ENTRY       VALUE IS 'MAN'.
              88  QUICK-ADD          VALUE IS 'QAD'.
              88  LOCATOR            VALUE IS 'LOC'.
           05 UHR-SALES-TAX-BREAKDOWN.
U             10  UHR-TOTAL-SALES-TAX    PIC 9(5)V99.
U             10  UHR-TTL-SLS-TAX-SIGN   PIC X.
U             10  UHR-TAX-BUCKET-2       PIC 9(5)V99.
U             10  UHR-TAX-BKT-2-SIGN     PIC X.
U             10  UHR-TAX-BUCKET-3       PIC 9(5)V99.
U             10  UHR-TAX-BKT-3-SIGN     PIC X.
U             10  UHR-TAX-BUCKET-4       PIC 9(5)V99.
U             10  UHR-TAX-BKT-4-SIGN     PIC X.
U          05 UHR-REC-DATE           PIC 9(8).
U          05 UHR-VOID-BY-TRACER     PIC 9999.
           05 UHR-VOID-REASON        PIC XX.
           05 UHR-PETTY-CASH-MEMO    PIC X(40).
           05 UHR-GEN-MEMO REDEFINES UHR-PETTY-CASH-MEMO PIC X(40).
U          05 UHR-REF-DATE           PIC 9(8).
           05 UHR-TRAINING-FLAG      PIC X(1).
              88  THIS-IS-TRAINING   VALUE IS 'Y'.
           05 UHR-SA-VOID-IND        PIC 9.
           05 UHR-REF-TERM           PIC 9999.
U          05 UHR-REF-TRACER         PIC 9999.
           05 UHR-REF-STORE          PIC 9999.
           05 UHR-RET-FROM.
              10 UHR-RET-FROM-TERM   PIC 9(4).
              10 UHR-RET-FROM-TRACER PIC 9(4).
U          05 UHR-TRAN-TOTAL-2D      PIC 9(7)V99.
U          05 UHR-TRAN-TOTAL-SIGN    PIC X.
U          05 UHR-OP-CODE            PIC 9(3).
U          05 UHR-DIVISION           PIC 9(3).
U          05 UHR-NAME-PLATE         PIC 9(3).
           05 UHR-MGR-ID             PIC X(8).
           05 UHR-AMEX-TNDR          PIC X(1).
           05 UHR-BNKCRD-TNDR        PIC X(1).
           05 UHR-HOUSE-TNDR         PIC X(1).
           05 UHR-ASSOC-ERROR        PIC X(1).
           05 UHR-VERIFY-ID          PIC X(8).
U          05 UHR-VERIFY-DATE        PIC 9(8).
           05 UHR-TLOG-VERSION       PIC X(2).
U          05 UHR-WEB-ORDER-NUM      PIC 9(9).
           05 UHR-CRITICAL-ERROR     PIC X(1).
              88  THIS-HAS-CRITICAL  VALUE IS 'Y'.
           05 UHR-FIFTH-AVENUE-CLUB  PIC 999.
           05 UHR-INTL-SHIP          PIC X(4).
U          05 UHR-POS-MGR            PIC 9(7).
           05 UHR-TNDR-OVERRIDE      PIC X(1).
U          05 UHR-GIFT-MGR           PIC 9(7).
           05 UHR-GIFT-SEND          PIC X.
           05 UHR-TRE-RESPONSE       PIC X.
           05 UHR-TRE-OVERRIDE       PIC X.
           05 UHR-FULFILL-STORE      PIC 9(4).
           05 UHR-PRINT-EMAIL        PIC X.
              88  PRINT-ONLY         VALUE 'P'.
              88  EMAIL-ONLY         VALUE 'E'.
              88  PRINT-AND-EMAIL    VALUE 'B'.
           05 UHR-LOTTO-NBR          PIC X(10).
           05 UHR-DRAW-NBR           PIC 9(03).
           05 UHR-LOTTO-DRAW-DT      PIC 9(08).
           05 UHR-LOTTO-CAL-VER      PIC 9(04).
U          05 UHR-WEB-ORDER-DT       PIC 9(8).
           05 UHR-WEB-ORDER-TIME     PIC 9(6).
           05 UHR-ORDER-NUM          PIC 9(9).
           05 UHR-ORDER-TYPE         PIC X.
           05 UHR-PCAP-CARD-NUM      PIC 9(16).
           05 UHR-PCAP-CARD-NUM-X REDEFINES
              UHR-PCAP-CARD-NUM      PIC X(16).
           05 UHR-WEB-SUFFIX         PIC 9(3).
U          05 UHR-WEB-ORDER-NO       PIC 9(9).
           05 UHR-LOCATOR-FIELDS.
               06 UHR-LOC-ORDER-NO        PIC 9(8).
               06 UHR-LOC-SHIPMENT-NUM    PIC 99.
               06 UHR-LOC-LINES-PRESENT   PIC X(10).
               06 UHR-LOC-HOLD-ORDER-TYPE PIC X(03).
               06 UHR-LOC-HOLD-ITEM-COUNT PIC 9(04).
U          05 UHR-REPLAYED-INVOICE   PIC X.
U          05 UHR-FILLER4            PIC X(13).
      *
      *-------------------------------------------------------------
      *
       01  UNPACKED-DETAIL-RECORD REDEFINES UNPACKED-HEADER-RECORD.
           05 UDR-KEY            PIC X(27).
           05 UDR-RECTYPE        PIC 99.
U          05 UDR-DEPT-CLASS     PIC 9(7).
U          05 UDR-VENDOR         PIC 9(9).
U          05 UDR-SKU            PIC 9(7).
      * FOR MCRAE'S AND PROFFITT'S, THIS IS THE 5-DIGIT SKU
      * FOR YOUNKERS, IT IS THE 7 DIGIT PARIS SKU
      * CPS USES UDR-EVERYDAY-PRICE AND UDR-OVERRIDE-PRICE
U          05 UDR-EVERYDAY-PRICE PIC 9(7)V99.
           05 UDR-RMGT-PRICE     REDEFINES UDR-EVERYDAY-PRICE
U                                PIC 9(7)V99.
U          05 UDR-MARKDOWN REDEFINES
              UDR-RMGT-PRICE     PIC 9(9).
           05 UDR-MARKDOWN-2D REDEFINES UDR-MARKDOWN
U                                PIC 9(7)V99.
U          05 UDR-MARKDOWN-SIGN  PIC X.
U          05 UDR-PRICE          PIC 9(9).
           05 UDR-PRICE-2D REDEFINES UDR-PRICE
U                                PIC 9(7)V99.
U          05 UDR-PRICE-SIGN     PIC X.
U          05 UDR-OVERRIDE-PRICE PIC 9(7)V99.
           05 UDR-PRICE-B4-OVERRIDE REDEFINES UDR-OVERRIDE-PRICE
U                                PIC 9(7)V99.
U          05 UDR-DISCOUNT REDEFINES
              UDR-PRICE-B4-OVERRIDE PIC 9(9).
           05 UDR-DISCOUNT-2D REDEFINES UDR-DISCOUNT
U                                   PIC 9(7)V99.
U          05 UDR-DISCOUNT-SIGN  PIC X.
      * FOR ENHANCED SALES AUDIT, ALL DIVISIONS USE UDR-QTY-0D
U          05 UDR-QTY-0D         PIC 9(5).
U          05 UDR-QTY-3D REDEFINES UDR-QTY-0D PIC 9(2)V999.
           05 UDR-DELETE         PIC X.
           05 UDR-UPC-NO-13      PIC 9(13).
           05 FILLER1 REDEFINES UDR-UPC-NO-13.
              07  FILLER2        PIC 9.
              07  UDR-UPC-NUMBER PIC 9(12).
           05 UDR-INDEX          PIC 9(13).
           05 UDR-PRANG-YNK-SKU
              REDEFINES UDR-INDEX PIC 9(13).
           05 UDR-CPS-SKU REDEFINES UDR-INDEX PIC 9(13).
U          05 UDR-ASSOC-DISCOUNT  PIC 9(7)V99.
U          05 UDR-ASSOC-DISC-SIGN PIC X.
      *  0 = TAXABLE; 1 = NON-TAXABLE
           05 UDR-TAX-FLAG       PIC X.
           05 UDR-SPA-RTN-OVERRD-FL PIC X.
              88  SPA-DATE-OVERRIDE    VALUE IS 'D'.
              88  SPA-PRICE-OVERRIDE   VALUE IS 'P'.
              88  RETURN-DAYS-OVERRIDE VALUE IS 'R'.
              88  JEWELRY-DAYS-OVERRIDE VALUE IS 'J'.
              88  MANUAL-DISCOUNT VALUE IS 'M'.
              88  CLEARANCE-RETURN VALUE IS 'F'.
U          05 UDR-ORIG-DTL-ASSOC PIC 9(7).
U          05 UDR-FREQ-SHOPPER   PIC 9(13).
      *  MAH  08-24-2011  NEED TO RE-USE THIS FILLER FOR ONLINE SAO300OL
           05 UDR-FILLER-COLOR   PIC 9(3).
           05 FILLER REDEFINES UDR-FILLER-COLOR.
              08 UDR-ORDER-NO    PIC X(02).
              08 UDR-LOCATOR-PRESELL PIC X(01).
      *  MAH  08-24-2011  **********************************************
           05 UDR-MKDN-PERCENT   PIC V99.
U          05 UDR-TICKET-PRICE   PIC 9(7)V99.
U          05 UDR-TKT-PRICE-SIGN PIC X.
           05 UDR-OVERRIDE-FLAG  PIC X.
           05 UDR-UPC-SCAN-FLAG  PIC X.
              88  UPC-SCANNED        VALUE 'Y'.
              88  UPC-NOT-SCANNED    VALUE ' ', 'N'.
           05 UDR-SEND-ZIP-CODE  PIC 9(5).
           05 UDR-CAN-POSTAL-CODE REDEFINES UDR-SEND-ZIP-CODE.
              10  UDR-CAN-POSTAL-CODE-3     PIC X(03).
              10  UDR-CAN-POSTAL-CODE-FILL  PIC X(02).
           05 UDR-TAX-OVERRIDE   PIC X.
              88  TAX-WAS-OVERRIDDEN VALUE 'Y'.
              88  NO-TAX-OVERRIDE    VALUE ' ', 'N'.
           05 UDR-VALUE-PRICED   PIC X.
              88  VALUE-PRICED-ITEM  VALUE 'Y'.
           05 UDR-PRICE-STATUS-FLAG  PIC X.
           05 UDR-ZERO-RETURN-IND    PIC X(1).
           05 UDR-ASSOC-OR-FIRST-DAY PIC X.
           05 UDR-ELITE-SHIPPING     PIC X.
           05 UDR-GIFT-ITEM          PIC X.
           05 UDR-SALES-ASSOC-INFORMATION.
U             10  UDR-PRIMARY-SALES-ASSOC    PIC 9(7).
U             10  UDR-ASSISTING-SALES-ASSOC  PIC 9(7).
U             10  UDR-RINGING-SALES-ASSOC    PIC 9(7).
U             10  UDR-MAKEUP-ARTIST-ASSOC    PIC 9(7).
           05 UDR-ORIGINAL-TRANS-INFO.
U             10  UDR-ORIGTRN-DATE           PIC 9(8).
              10  UDR-ORIGTRN-STORE          PIC 9999.
              10  UDR-ORIGTRN-TERM           PIC 9999.
U             10  UDR-ORIGTRN-TRACER         PIC 9999.
              10  FILLER                     PIC 9.
              10  UDR-ORIGTRN-RECEIPT-FOUND  PIC X.
              10  UDR-RETURN-REASON          PIC XX.
U             10  UDR-ORIGTRN-PRI-SA-ASSOC   PIC 9(7).
U             10  UDR-ORIGTRN-AST-SA-ASSOC   PIC 9(7).
              10  UDR-FULFILL-STORE          PIC 9(4).
U             10  UDR-ORIGTRN-MKUP-SA-ASSOC  PIC 9(7).
           05 UDR-SALES-TAX.
              10  UDR-TAX-EXEMPT-ID           PIC X(20).
              10  UDR-TAX-EXEMPT-REASON       PIC XX.
U             10  UDR-ITEM-TOTAL-TAX          PIC 9(5)V99.
U             10  UDR-ITEM-TTL-TAX-SIGN       PIC X.
U             10  UDR-ITEM-STATE-TAX          PIC 9(5)V999.
U             10  UDR-ITEM-STATE-TAX-SIGN     PIC X.
U             10  UDR-ITEM-COUNTY-TAX         PIC 9(5)V999.
U             10  UDR-ITEM-COUNTY-TAX-SIGN    PIC X.
U             10  UDR-ITEM-LOCAL-TAX          PIC 9(5)V999.
U             10  UDR-ITEM-LOCAL-TAX-SIGN     PIC X.
           05  UDR-TRAN-CATEGORY              PIC X(3).
              88  SALE                VALUE IS 'SL '.
              88  SALES-RETURN        VALUE IS 'RTN'.
              88  SALE-PRICE-ADJ      VALUE IS 'SPA'.
              88  INTERBRANCH-XREF    VALUE IS 'IBT'.
              88  MARK-OUT-STK        VALUE IS 'MOS'.
              88  RTN-TO-VENDOR       VALUE IS 'RTV'.
              88  GENERAL-LEDGER      VALUE IS 'GL '.
              88  PETTY-CASH          VALUE IS 'PC '.
              88  TRAVEL-ADVANCE      VALUE IS 'TA '.
              88  PAYMENT-ON-ACCOUNT  VALUE IS 'POA'.
           05 UDR-CUST-INFO-NO        PIC 99.
           05 UDR-USER-ID             PIC X(8).
U          05 UDR-CHANGE-DATE         PIC 9(8).
           05 UDR-LWP-STORE-ON-OFF    PIC X.
           05 UDR-LWP-USED            PIC X.
           05 UDR-LWP-ASSOC-OVERRIDE  PIC X.
           05 UDR-COUPON-DOC-GL       PIC X(16).
           05 UDR-COUPON-NO REDEFINES UDR-COUPON-DOC-GL
                                      PIC X(16).
           05 UDR-DOC-GL    REDEFINES UDR-COUPON-DOC-GL
                                      PIC X(16).
           05 UDR-AUTH-CODE           PIC X(8).
U          05 UDR-GOW                 PIC 9(9).
U          05 UDR-GIFTREG             PIC 9(9).
U          05 UDR-BIG-TICKET          PIC 9(9).
U          05 UDR-CENT-STK            PIC 9(9).
           05 UDR-DOC-GL-TYP          PIC X(1).
              88  DOCUMENT-NO         VALUE IS 'D'.
              88  GL-NO               VALUE IS 'G'.
              88  COUPON-NO           VALUE IS 'C'.
              88  JEWELRY-CASE        VALUE IS 'J'.
              88  ALOHA-HOUSE         VALUE IS 'H'.
           05 UDR-TAX-EXEMPT-IND      PIC X.
           05 UDR-EGC-CODE            PIC 9(4).
           05 UDR-SPECIAL-SERVICE-IND PIC X(3).
              88  ALTERATION-SPECIAL-USE       VALUE 'ASP'.
              88  MONOGRAMMING-SPECIAL-SERVICE VALUE 'MSS'.
              88  REPAIR-SPECIAL-SERVICE       VALUE 'RSS'.
              88  ENGRAVING-SPECIAL-SERVICE    VALUE 'ESS'.
           05 UDR-MDSE-STATUS-FLAG    PIC 9(4).
      *
      *       UDR-MDSE-STATUS-FLAG VALUES
      *       0001 - REGULAR
      *       0002 - MARKDOWN
      *       0004 - CLEARANCE
      *       0008 - FINAL (YELLOW DOT)
      *       0010 - VALUE PRICE
      *
           05 UDR-MDSE-IND            PIC X(3).
U          05 UDR-ASSISTING-SALES-ASSOC-ST  PIC 9(3).
U          05 UDR-LOC-ORDER-LINENUM         PIC 9(3).
           05 UDR-MAKEUP-ARTIST-ASSOC-ST REDEFINES
U             UDR-LOC-ORDER-LINENUM         PIC 9(3).
U          05 UDR-ORIGTRN-PRI-SA-ASSOC-ST   PIC 9(3).
U          05 UDR-ORIGTRN-AST-SA-ASSOC-ST   PIC 9(3).
           05 UDR-REPLENISH-ITEM-FLAG       PIC X.
              88 UDR-REPLENISH-ITEM         VALUE 'Y'.
U          05 UDR-REPLENISH-DAYS            PIC 9(3).
U          05 UDR-SD-PRIMARY-SALES-ASSOC-ST  PIC 9(3).
           05 UDR-UPC-NOT-ON-FILE-FLAG      PIC X(1).
              88  UPC-NOT-ON-FILE    VALUE 'Y'.
              88  UPC-ON-FILE        VALUE ' ', 'N'.
           05 UDR-BOGO-DRIVER               PIC X(1).
           05 UDR-BOGO-PASSENGER            PIC X(1).
           05 UDR-BOGO-GROUP                PIC X(2).
           05 UDR-BOGO-COUPON-IND           PIC X(1).
           05 UDR-REASON2-OVERRIDE          PIC X(1).
U          05 UDR-MANUAL-DISCOUNT-ASSOC     PIC 9(7).
           05 UDR-FASHION-FIX               PIC X(1).
           05 UDR-SAKS-DIRECT-ITEM-FLG      PIC X(1).
           05 UDR-LIGHTNING                 PIC X(1).
           05 UDR-SERIAL-NUMBER             PIC X(15).
           05 UDR-RETURNABLE                PIC X.
           05 UDR-ITEM-DESC                 PIC X(40).
           05 UDR-WEB-LINE-NUM              PIC 9(9).
           05 UDR-WEB-LINE-NUM-X REDEFINES
              UDR-WEB-LINE-NUM              PIC X(9).
           05 UDR-NON-MERCH-FLAG            PIC X(01).
           05 UDR-FILLER4                   PIC X(01).
      *
      *---------------------------------------------------------------
      *  RECORD 40 = DEPOSIT
      *
       01  UNPACKED-FEE-RECORD REDEFINES UNPACKED-HEADER-RECORD .
           05 UFR-KEY            PIC X(27).
           05 UFR-RECTYPE        PIC 99.
           05 UFR-CODE           PIC 9(5).
U          05 UFR-AMOUNT         PIC 9(9).
U          05 UFR-AMOUNT-2D REDEFINES UFR-AMOUNT PIC 9(7)V99.
U          05 UFR-AMOUNT-SIGN    PIC X.
           05 UFR-DELETE         PIC X.
           05 UFR-FILLER1        PIC X(467).
      *---------------------------------------------------------------
      ***ADD FOR ENHANCED SALES AUDIT :PHASE ONE:
      *  TENDER RECORD TYPE = 50
      *
       01  UNPACKED-TENDER-RECORD REDEFINES UNPACKED-HEADER-RECORD .
           03 UTR-KEY            PIC X(27).
           03 UTR-TEND-DATA.
           05 UTR-RECTYPE        PIC 99.
           05 UTR-TENDER-TYPE    PIC 999.
              88 CASH            VALUE IS 001.
              88 HOUSE-CHARGE    VALUE IS 011, 030, 032, 036,
                      038, 040, 041, 042, 044.
              88 BANK-CHARGE    VALUE IS 005, 006, 007, 008,
                      009, 010, 053.
           05 UTR-CUST-NO        PIC 99.
           05 UTR-ACCOUNT-NO     PIC X(32).
           05 UTR-EXPIRATION-DATE PIC 9(4).
           05 UTR-MODE-OF-ENTRY  PIC X.
              88  CARD-SWIPED         VALUE IS 'S'.
              88  CARD-BAR-CODE-SCAN  VALUE IS 'B'.
              88  CARD-KEYED          VALUE IS 'K'.
           05 UTR-AUTH-RESPONSE.
              10  UTR-AUTH-RESPONSEN  PIC 9(2).
              10  UTR-AUTH-CODE       PIC X(8).
           05 UTR-FILLER              PIC X(40).
U          05 UTR-TENDER-AMOUNT       PIC 9(7)V99.
U          05 UTR-TENDER-AMT-SIGN     PIC X.
           05 UTR-FLOOR-LIMIT-INDC    PIC X.
              88  FLOOR-LIMIT-ACTIVE  VALUE IS 'Y'.
           05 UTR-CUST-INFO.
              10  UTR-CUSTOMER-ID-TYPE    PIC X(1).
                  88  DRIVERS-LICENSE    VALUE IS 'D'.
                  88  MILITARY-ID        VALUE IS 'M'.
                  88  STATE-ID           VALUE IS 'S'.
                  88  PASSPORT-NO        VALUE IS 'P'.
              10  UTR-CUSTOMER-DL-NUMB    PIC X(22).
              10  UTR-CUSTOMER-MICR       PIC X(26).
              10  UTR-CUSTOMER-STATE      PIC X(3).
              10  UTR-CUSTOMER-ZIP-C      PIC X(10).
              10  UTR-CUSTOMER-PHONE      PIC 9(10).
           05 UTR-LOYALTY-IND    PIC X.
              88  LOYALTY-IND        VALUE IS 'Y'.
           05 UTR-PLAN-CODE      PIC 99.
           05 UTR-POA-NO         PIC X(16).
           05 UTR-CUST-NAME      PIC X(30).
           05 UTR-DELETE         PIC X.
           05 UTR-CHANGE-USERID  PIC X(8).
U          05 UTR-CHANGE-DATE    PIC 9(8).
           05 UTR-EGC-CODE       PIC 9(4).
           05 UTR-DCC            PIC X(1).
                  88  NA           VALUE IS ' '.
                  88  ACCEPTED     VALUE IS 'A'.
                  88  DECLINED     VALUE IS 'D'.
           05 UTR-EMPLOYEE-DISC  PIC X(1).
                  88  EMPLOYEE          VALUE IS 'E'.
                  88  FRIENDS           VALUE IS 'B'.
           05 UTR-EMP-NUM        PIC 9(7).
           05 UTR-INVALID-TND-NO PIC X(1).
           05 UTR-AMEX-TRANS-IDENT   PIC 9(15).
           05 UTR-AMEX-POSDATA-CODE  PIC X(12).
           05 UTR-AVS-RESP           PIC X(01).
           05 UTR-CID-RESP           PIC X(01).
U          05 UTR-REF-DATE           PIC 9(8).
           05 UTR-REF-STORE          PIC 9999.
           05 UTR-REF-TERM           PIC 9999.
U          05 UTR-REF-TRACER         PIC 9999.
           05 UTR-DEP-COUP-OVRD      PIC X.
           05 UTR-COUNTRY-CDE        PIC X(02).
           05 UTR-CURRENCY-CDE       PIC 9(03).
           05 UTR-NEW-PLAN           PIC X(5).
U          05 UTR-RMGT-TEND-AMT      PIC 9(7)V99.
U          05 UTR-RMGT-TEND-AMT-SIGN PIC X.
           05 UTR-TOKEN-FLAG         PIC 9.
           05 UTR-TOKEN-POA-FLAG     PIC 9.
           05 UTR-APPLE-PAY-IND      PIC X.
           05 UTR-FILLER1            PIC X(155).
      *
      *---------------------------------------------------------------
      *  60 RECORD = MEMO - CURRENTLY USED TO TRACK MOVS, GIFT CERTS
      *              AND EGCS ISSUED AND REDEEMED.
      *              ALSO USED TO TRACK RESERVATION NUMBER
      *              FOR BIG TICKET AND CENTRAL STOCK, AND GIFT
      *              REGISTRY NUMBER.
      *              USED INTERNALLY BY JACKSON TO HOLD RETURN AND
      *              SALE PORTION OF SALES TAX NEEDED TO EXPLODE
      *              EXCHANGE TRANSACTIONS BACK INTO A SALE AND
      *              RETURN (FOR CPS ONLY).
      *
       01  UNPACKED-MEMO-RECORD REDEFINES UNPACKED-HEADER-RECORD .
           05 UMR-KEY            PIC X(27).
           05 UMR-RECTYPE        PIC 99.
           05 UMR-DEPT-CLASS     PIC 9(6).
           05 UMR-DATA-TYPE REDEFINES UMR-DEPT-CLASS PIC 9(6).
      *
      * 00001 IN UMR-DATA-TYPE INDICATES GIFT REGISTRY.
      * 00002 IN UMR-DATA-TYPE INDICATES CENTRAL STOCK.
      * 00003 IN UMR-DATA-TYPE INDICATES BIG TICKET RESERVATION.
      * 00004 IN UMR-DATA-TYPE INDICATES EXCHANGE TAX (CPS ONLY).
      * 00005 IN UMR-DATA-TYPE INDICATES EXCHANGE TAX (CPS ONLY).
      * 00006 IN UMR-DATA-TYPE INDICATES INTERNET SALES.
      *
U          05 UMR-NUMBER         PIC 9(18).
U          05 UMR-AMOUNT         PIC 9(7)V99.
U          05 UMR-AMOUNT-SIGN    PIC X.
           05 FILLER3.
              10  UMR-YNKR-QUAL  PIC XX.
U             10  UMR-RINGING-ASSOC  PIC 9(7).
           05 UMR-DELETE         PIC X.
           05 UMR-VOID-FLAG      PIC X.
      * UMR-DATA AND UMR-DATA-TYPE ARE USED FOR CPS.
      * UMR-AMOUNT2 IS USED INTERNALLY BY JACKSON TO HOLD
      *     ASSOCIATE DISCOUNT ON TAX FOR SALE AND RETURN
      *     PORTIONS OF AN EXCHANGE.
           05 UMR-DATA           PIC X(60).
           05 UMR-FILLER2 REDEFINES UMR-DATA.
              10  UMR-REGISTRY-NO    PIC 9(7).
              10  UMR-NAME           PIC X(30).
              10  UMR-FILLER3        PIC X(23).
           05 UMR-BIG-TICKET-DATA REDEFINES UMR-DATA.
              10  UMR-BIG-TICKET-NUM PIC 9(9).
              10  FILLER             PIC X(51).
           05 UMR-EGC-DATA REDEFINES UMR-DATA.
              10  UMR-EGC-VALUE-LINK-CODE PIC 9(4).
              10  UMR-EGC-AUTH-CODE       PIC 9(6).
              10  FILLER                  PIC X(50).
U          05 UMR-AMOUNT2        PIC 9(7)V99.
U          05 UMR-AMOUNT2-SIGN   PIC X.
           05 UMR-LOYALTY-CLUB   PIC X.
           05 UMR-FILLER1        PIC X(367).
      *
      *-------------------------------------------------------------
      *
      *  CUSTOMER INFORMATION RECORDS TYPE 70
      *
       01  UNPACKED-CUSTOMER-RECORD REDEFINES UNPACKED-HEADER-RECORD .
           05 UCR-KEY            PIC X(27).
           05 UCR-RECTYPE        PIC 99.
           05 UCR-CUST-INFO-NO   PIC 99.
           05 UCR-ADDRESS-TYPE   PIC XX.
              88 CUSTOMER-INFO        VALUE IS 'CI'.
              88 CUSTOMER-RETURN      VALUE IS 'RT'.
              88 CUSTOMER-SEND-TO     VALUE IS 'ST'.
              88 CUSTOMER-CLIENTBOOK  VALUE IS 'CB'.
           05 UCR-CUSTOMER-NAME      PIC X(30).
           05 UCR-CUSTOMER-ADDR-1    PIC X(30).
           05 UCR-CUSTOMER-ADDR-2    PIC X(30).
           05 UCR-CUSTOMER-CITY      PIC X(22).
           05 UCR-CUSTOMER-STATE     PIC X(3).
           05 UCR-CUSTOMER-ZIP       PIC X(10).
           05 UCR-CHANGE-USERID  PIC X(8).
U          05 UCR-CHANGE-DATE    PIC 9(8).
           05 UCR-DELETE             PIC X.
           05 UCR-MGR-ID             PIC X(8).
U          05 UCR-CLIENTBOOK-NUM     PIC 9(14).
           05 UCR-FIRST-NAME         PIC X(15).
           05 UCR-LAST-NAME          PIC X(25).
           05 UCR-COUNTRY            PIC X(20).
           05 UCR-PHONE              PIC X(16).
           05 UCR-SSN                PIC X(9).
           05 UCR-DOC-NUM            PIC X(8).
U          05 UCR-ASSOC-CREDIT       PIC 9(7).
U          05 UCR-ASSOC-CREDIT-AMT   PIC 9(7)V99.
U          05 UCR-ASSOC-CR-AMT-SIGN  PIC X.
           05 UCR-FILLER1            PIC X(205).
      *
      **********************************************************
      *  GIFT GIVER INFORMATION RECORDS TYPE 75
      *
       01  UNPACKED-GIFT-GIVER-RECORD REDEFINES UNPACKED-HEADER-RECORD.
           05 UGR-KEY            PIC X(27).
           05 UGR-RECTYPE        PIC 99.
           05 UGR-GIFT-GIVER-NAME    PIC X(25) OCCURS 10 TIMES.
           05 UGR-CHANGE-USERID  PIC X(8).
U          05 UGR-CHANGE-DATE    PIC 9(8).
           05 UGR-DELETE             PIC X.
           05 UGR-FILLER1            PIC X(216).
      *
      *-------------------------------------------------------------
      *  80 RECORD = EGC ACTIVATION INFORMATION USED TO STORE
      *              REFERENCE DATELINE
      *
       01  UNPACKED-EGC-ACT-RECORD REDEFINES UNPACKED-HEADER-RECORD .
           05 UEA-KEY            PIC X(27).
           05 UEA-RECTYPE        PIC 99.
           05 UEA-DATA-TYPE      PIC 9(6).
           05 UEA-EGC-NO         PIC X(16).
           05 UEA-EGC-CODE       PIC 9(4).
           05 UEA-AUTH-CODE      PIC X(8).
U          05 UEA-EGC-ZIP        PIC X(10).
           05 UEA-AUTH-DATELINE.
U             10 UEA-AUTH-DATE    PIC 9(8).
              10 UEA-AUTH-STORE   PIC 9999.
              10 UEA-AUTH-TERM    PIC 9999.
U             10 UEA-AUTH-TRACER  PIC 9999.
           05 UEA-DELETE          PIC X.
U          05 UEA-FILLER1         PIC X(418).
      *
      /*************************************************************
      *********    SALES FILE DESCRIPTION               ************
      **************************************************************
      *
      *        UHR-RECTYPE CODES ARE AS FOLLOWS:
      *
      *       RECORD TYPE         RECORD DESCRIPTION
      *           00              SALES HEADER RECORD.
      *           05              SALES LINE ITEM RECORD.
      *           40              FEE RECORD
      *           50              TENDER RECORD
      *           60              MEMO RECORD
      *           70              CUSTOMER INFORMATION RECORD
      *           75              GIFT GIVER INFORMATION RECORD
      *           80              EGC ACTIVATION INFORMATION RECORD
      *
