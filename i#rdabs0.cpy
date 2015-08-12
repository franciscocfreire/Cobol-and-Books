      *---------------------------------------------------------------* 
      * AREA     : I#RDABS0                                           * 
      * ALTERACAO: 31/08/2004                                         * 
      * TAMANHO  :                                                    * 
      * CRIADOR  : HENRIQUE / ADRIANO                                 * 
      * OBJETIVO : INCLUDE COM A FORMATACAO DO EXTRACT DO CACS        * 
      *            DERIVADA DA INCLUDE "P327EXTC" DO PACOTE DA AMS.   * 
      *---------------------------------------------------------------* 
                                                                        
       01  REG-BASE-EXTRACT.                                            
001000 03  ACTG-EXTRACT-SEG.                                            
001100     05 ACTG-EXTRACT-ALTERNATE-KEY.                               
001200        10 SEARCH-NAME        PIC X(15).                          
001300        10 ACTG-EXTRACT-SEGMENT-KEY.                              
001400                                                                  
001500          15 LOCATION-CODE        PIC X(6).                       
001600          15 ACCT-NUM             PIC X(18).                      
001700     05 ACTG-EXTRACT-SEGMENT-DATA.                                
001800      10 ON-LINE-SHARED-USE-DATA.                                 
004000       15 CUSTOMER-NAME       PIC X(45).                          
004100       15 CUST-FIRST-NAME     PIC X(25).                          
004200       15 CUST-MIDDLE-NAME    PIC X(25).                          
004300       15 CUST-LAST-NAME      PIC X(35).                          
004400       15 CUST-MATERNAL       PIC X(35).                          
004500       15 CUST-PREFIX         PIC X(04).                          
004600       15 CUST-SUFFIX         PIC X(04).                          
004700       15 CUSTOMER-ADDRESS.                                       
004800        20 CUSTOMER-ADDRESS-STREET                                
004900                              PIC X(35) OCCURS 3 TIMES.           
005000        20 CUSTOMER-ADDRESS-CITY-STATE.                           
005100           25 CUSTOMER-CITY   PIC X(30).                          
005200           25 CUSTOMER-STATE  PIC X(02).                          
005300        20 CUST-ZIP-POST-CODE PIC X(09).                          
005400       15 CUSTOMER-COUNTY     PIC X(30).                          
005500       15 CUST-ISO-COUNTRY-CODE                                   
005600                              PIC X(02).                          
005700       15 CUST-EMAIL-ADDRESS  PIC X(48).                          
005800       15 PRI-LANGUAGE-IND    PIC X(02).                          
006032       15 EXT-CUST-REF-NUM      PIC X(03).                        
006031       15 SOCIAL-SECURITY-NUM PIC X(20).                          
006032       15 EXT-CUST-TAX-ID-TYPE  PIC X(01).                        
006200       15 PRIMARY-CUST-REL    PIC X(1).                           
006300       15 PRIMARY-ADDR-REL    PIC 9(1).                           
007300       15 SECONDARY-CUST-NAME PIC X(45).                          
007400       15 PRIMARY-BUSINESS-NAME                                   
007500                              PIC X(45).                          
007800       15 PRIMARY-BUSINESS-REL                                    
007900                              PIC X(1).                           
008000       15 PRIMARY-BUS-ADDR-REL                                    
008100                              PIC 9(1).                           
008200       15 PRIMARY-BUSINESS-ADDRESS.                               
010100        20 BUSINESS-ADDRESS-STREET                                
010200                              PIC X(35) OCCURS 3 TIMES.           
010300        20 BUSINESS-ADDRESS-CITY-STATE.                           
010400         25 BUSINESS-CITY     PIC X(30).                          
010500         25 BUSINESS-STATE    PIC X(02).                          
010600        20 BUS-ZIP-POST-CODE  PIC X(09).                          
010700       15 BUSINESS-COUNTY     PIC X(30).                          
010800       15 BUS-ISO-COUNTRY-CODE                                    
010900                              PIC X(02).                          
011000       15 BUSINESS-EMAIL-ADDRESS                                  
011100                              PIC X(48).                          
011200       15 BUS-LANGUAGE-IND     PIC X(02).                         
006032       15 EXT-BUS-REF-NUM         PIC X(03).                      
006032       15 BUS-SOCIAL-SEC-NUM      PIC X(20).                      
006032       15 EXT-BUS-TAX-ID-TYPE PIC X(01).                          
011500       15 TELEPHONE-NUMBER-DATA.                                  
011542        20 PHONE-NUMBER-DATA OCCURS 6 TIMES.                      
011700         25 PHONE-TYPE-CODE    PIC X.                             
011800         25 PHONE-AVAILABILITY-CODE                               
011900                              PIC X.                              
012000         25 PHONE-NUMBER.                                         
012100          30 PHONE-AREA-CODE   PIC X(3).                          
012200          30 PHONE-PREFIX      PIC X(3).                          
012300          30 PHONE-SUFFIX      PIC X(4).                          
013800          30 PHONE-EXTENSION        PIC X(7).                     
013900         25 EXT-PHONE-NUMBER-FMT    PIC X(02).                    
014000       15 CREDIT-LIMIT-AMT          PIC S9(12) COMP-3.            
014100                                                                  
014400       15 EXPIRATION-DATE     PIC 9(6) COMP-3.                    
015900       15 BALANCE-AMT         PIC S9(12)V9(6) COMP-3.             
016000       15 OVERLIMIT-AMT       PIC S9(12)V9(6) COMP-3.             
016100       15 DISPUTED-AMT        PIC S9(12)V9(6) COMP-3.             
016200       15 TOTAL-DUE-AMT       PIC S9(12)V9(6) COMP-3.             
016300       15 CURRENT-DUE-AMT     PIC S9(12)V9(6) COMP-3.             
016400       15 COLLECTOR-ASSIGN-UD-1                                   
016500                              PIC X(12).                          
016600       15 COLLECTOR-ASSIGN-UD-1-TYPE                              
016700                              PIC X(02).                          
016800       15 COLLECTOR-ASSIGN-UD-1-FMT                               
016900                              PIC 9(02).                          
017000       15 COLLECTOR-ASSIGN-UD-2    PIC X(12).                     
017100       15 COLLECTOR-ASSIGN-UD-2-TYPE                              
017200                              PIC X(02).                          
017300       15 COLLECTOR-ASSIGN-UD-2-FMT                               
017400                              PIC 9(02).                          
017500       15 TOTAL-DELINQ-AMT    PIC S9(12)V9(6) COMP-3.             
017800       15 CYCLES-DELINQUENT-CNT                                   
017900                              PIC S99 COMP-3.                     
018601       15 DAYS-DELINQUENT-NUM PIC S9(03) COMP-3.                  
018700       15 LAST-PAYMENT-AMT    PIC S9(12)V9(6) COMP-3.             
019000       15 CHARGE-OFF-DATE     PIC 9(6) COMP-3.                    
019600       15 DELQ-DAYS-AT-CHARGE-OFF                                 
019600                              PIC S9(03) COMP-3.                  
019800       15 USER-DEFINED-7      PIC X(12).                          
019900       15 USER-DEFINED-7-TYPE PIC X(02).                          
020000       15 USER-DEFINED-7-FMT  PIC 9(02).                          
020300       15 ACCT-OPEN-DATE      PIC 9(6).                           
020400       15 FILLER         REDEFINES ACCT-OPEN-DATE.                
020500        20 ORIGIN-YEAR        PIC 99.                             
020600        20 ANNIV-MONTH        PIC 99.                             
020700        20 ANNIV-DAY          PIC 99.                             
020800*                                                                 
020900       15 IL-SHARED-DATA.                                         
021000        20 NEXT-DUE-DATE      PIC 9(6) COMP-3.                    
022200        20 INTEREST-RATE      PIC S9(3)V9(3) COMP-3.              
022300        20 OTH-DELQ-AMT       PIC S9(12)V9(6) COMP-3.             
022400        20 REPOSSESSION-CHARGES                                   
022500                              PIC S9(12)V9(6) COMP-3.             
022800        20 REPOSSESSION-DATE  PIC 9(6) COMP-3.                    
022900        20 MATURITY-DATE      PIC 9(6) COMP-3.                    
023900        20 PAYMENT-AMT        PIC S9(12)V9(6) COMP-3.             
024000        20 PRINCIPAL-DELQ-AMT PIC S9(12)V9(6) COMP-3.             
024100        20 INTEREST-DELQ-AMT  PIC S9(12)V9(6) COMP-3.             
024400*                                                                 
024500      10 ALPHA-SEARCH-DATA.                                       
024600       15 USER-DEFINED-SEARCH-1                                   
024700                             PIC X(10).                           
025300       15 USER-DEFINED-SEARCH-1-TYPE                              
025400                             PIC X(02).                           
025500       15 USER-DEFINED-SEARCH-1-FMT                               
025600                             PIC 9(02).                           
025900       15 USER-DEFINED-SEARCH-2                                   
026000                             PIC X(10).                           
026600       15 USER-DEFINED-SEARCH-2-TYPE                              
026700                             PIC X(02).                           
026800       15 USER-DEFINED-SEARCH-2-FMT                               
026900                             PIC 9(02).                           
027200      10 ON-LINE-DISPLAY-DATA.                                    
027300       15 ACCT-NUM-SORT       PIC X(7).                           
027400       15 CYCLE-ID            PIC 99.                             
027500       15 UD-DATE-FIELD-1     PIC 9(6).                           
027600       15 LAST-BILLING-DATE   PIC 9(6) COMP-3.                    
027700       15 AGED-HISTORY        PIC X(12).                          
027800       15 CREDIT-LIMIT-DATE   PIC 9(6) COMP-3.                    
028800       15 INT-AND-LATE-CHG-AMT                                    
028900                              PIC S9(12)V9(6) COMP-3.             
029000       15 DELINQ-AMT-BY-AGE   PIC S9(12)V9(6) COMP-3 OCCURS 4.    
029300       15 NUMBER-OF-CARDS     PIC X(2).                           
029400       15 WARNING-BULLETIN-DATE                                   
029500                              PIC 9(6) COMP-3.                    
029600       15 WARNING-BULLETIN-ZONE                                   
029700                              PIC X(4).                           
029800       15 OTHER-ACCT-NUM      PIC X(18).                          
029900       15 CREDIT-SCORE        PIC X(5).                           
030000       15 BEHAVIOR-INDEX      PIC X(4).                           
030100       15 LAST-PAYMENT-DATE   PIC 9(6) COMP-3.                    
030900       15 LAST-MONETARY-AMT   PIC S9(12)V9(6) COMP-3.             
031200       15 LAST-MONETARY-DATE  PIC 9(6) COMP-3.                    
031300       15 LAST-MONETARY-TYPE  PIC XX.                             
033200       15 CHARGE-OFF-AMT      PIC S9(12)V9(6) COMP-3.             
033300       15 SORT-VALUE-DATA.                                        
033400        20 SORT-VALUE-1       PIC 9(12).                          
033500        20 SORT-VALUE-1-TYPE  PIC X(02).                          
033600        20 SORT-VALUE-1-FMT   PIC 9(02).                          
033700        20 SORT-VALUE-2       PIC X(12).                          
033800        20 SORT-VALUE-2-TYPE  PIC X(02).                          
033900        20 SORT-VALUE-2-FMT   PIC 9(02).                          
034000       15 USER-DEFINED-DATA.                                      
034100        20 USER-DEFINED-1          PIC X(17).                     
034200        20 USER-DEFINED-1-TYPE     PIC X(02).                     
034300        20 USER-DEFINED-1-FMT      PIC 9(02).                     
034400        20 USER-DEFINED-2          PIC X(30).                     
034500        20 USER-DEFINED-2-TYPE     PIC X(02).                     
034600        20 USER-DEFINED-2-FMT      PIC 9(02).                     
034700        20 USER-DEFINED-3          PIC X(14).                     
034800        20 USER-DEFINED-3-TYPE     PIC X(02).                     
034900        20 USER-DEFINED-3-FMT      PIC 9(02).                     
035000        20 USER-DEFINED-4          PIC X(79).                     
035100        20 USER-DEFINED-4-TYPE     PIC X(02).                     
035200        20 USER-DEFINED-4-FMT      PIC 9(02).                     
035300        20 USER-DEFINED-5          PIC X(79).                     
035400        20 USER-DEFINED-5-TYPE     PIC X(02).                     
035500        20 USER-DEFINED-5-FMT      PIC 9(02).                     
035600        20 USER-DEFINED-6          PIC X(12).                     
035700        20 USER-DEFINED-6-TYPE     PIC X(02).                     
035800        20 USER-DEFINED-6-FMT      PIC 9(02).                     
035900        20 USER-DEFINED-8          PIC X(79) OCCURS 5 TIMES.      
036000        20 USER-DEFINED-8-TYPE     PIC X(02) OCCURS 5 TIMES.      
036100        20 USER-DEFINED-8-FMT      PIC 9(02) OCCURS 5 TIMES.      
036400       15 IL-DISPLAY-DATA.                                        
037400        20 PRINCIPAL-AMT      PIC S9(12)V9(6) COMP-3.             
037500        20 OTHER-AMT          PIC S9(12)V9(6) COMP-3.             
037600        20 ORIG-BALANCE-AMT   PIC S9(12)V9(6) COMP-3.             
037900        20 OFFICER-NUM        PIC X(9).                           
038000        20 BRANCH-NUM         PIC X(6).                           
038100        20 ORIGINAL-TERM      PIC X(4).                           
039000        20 ORIG-INTEREST-RATE PIC S9(3)V9(3) COMP-3.              
039300        20 DEALER-NUM         PIC X(7).                           
039400        20 PAYMENT-FREQUENCY  PIC X(2).                           
039500        20 REFINANCE-DATE     PIC 9(6) COMP-3.                    
039600      15 UTILITY-DISPLAY-DATA.                                    
039700        20  DEPOSIT-IND        PIC X.                             
039800        20  ON-BUDGET-IND      PIC X.                             
039900        20  DPA-IND            PIC X.                             
040000        20  CUT-OFF-DATE       PIC 9(6) COMP-3.                   
040100        20  CUT-OFF-CODE       PIC X(3).                          
040200        20  ACCOUNT-TYPE       PIC X(3).                          
040300        20  SERVICE-TYPE       PIC X(3).                          
041200        20  BUDGET-PAYMENT-AMT PIC S9(12)V9(6) COMP-3.            
041300        20  TERMINATE-AMT      PIC S9(12)V9(6) COMP-3.            
041600        20  TERMINATE-EXP-DATE PIC 9(6) COMP-3.                   
041700      10 STATE-ASSIGNMENT-DATA.                                   
042600         15 SAT-USER-DEFINED-1   PIC X(12).                       
042700         15 SAT-USER-DEFINED-1-TYPE                               
042800                                 PIC X(02).                       
042900         15 SAT-USER-DEFINED-1-FMT                                
043000                                 PIC 9(02).                       
043100         15 SAT-USER-DEFINED-2   PIC X(12).                       
043200         15 SAT-USER-DEFINED-2-TYPE                               
043300                                 PIC X(02).                       
043400         15 SAT-USER-DEFINED-2-FMT                                
043500                                 PIC 9(02).                       
043800      10 MULTIPLE-ACCT-PROCESSING-DATA.                           
043900         15 CUSTOMER-INFO-NUMBER PIC X(24).                       
044000      10 USER-DEFINED-LETTER-DATA.                                
045700         15 LETTER-USER-DEFINED-1   PIC X(30).                    
045800         15 LETTER-USER-DEFINED-1-TYPE                            
045900                                    PIC X(02).                    
046000         15 LETTER-USER-DEFINED-1-FMT                             
046100                                    PIC 9(02).                    
046200         15 LETTER-USER-DEFINED-2   PIC X(30).                    
046300         15 LETTER-USER-DEFINED-2-TYPE                            
046400                                    PIC X(02).                    
046500         15 LETTER-USER-DEFINED-2-FMT                             
046600                                    PIC 9(02).                    
046700         15 LETTER-USER-DEFINED-3   PIC X(30).                    
046800         15 LETTER-USER-DEFINED-3-TYPE                            
046900                                    PIC X(02).                    
047000         15 LETTER-USER-DEFINED-3-FMT                             
047100                                    PIC 9(02).                    
047200         15 LETTER-USER-DEFINED-4   PIC X(30).                    
047300         15 LETTER-USER-DEFINED-4-TYPE                            
047400                                    PIC X(02).                    
047500         15 LETTER-USER-DEFINED-4-FMT                             
047600                                    PIC 9(02).                    
047700         15 LETTER-USER-DEFINED-5   PIC X(30).                    
047800         15 LETTER-USER-DEFINED-5-TYPE                            
047900                                    PIC X(02).                    
048000         15 LETTER-USER-DEFINED-5-FMT                             
048100                                    PIC 9(02).                    
048200         15 LETTER-USER-DEFINED-6   PIC X(30).                    
048300         15 LETTER-USER-DEFINED-6-TYPE                            
048400                                    PIC X(02).                    
048500         15 LETTER-USER-DEFINED-6-FMT                             
048600                                    PIC 9(02).                    
048700         15 LETTER-USER-DEFINED-7   PIC X(30).                    
048800         15 LETTER-USER-DEFINED-7-TYPE                            
048900                                    PIC X(02).                    
049000         15 LETTER-USER-DEFINED-7-FMT                             
049100                                    PIC 9(02).                    
049200         15 LETTER-USER-DEFINED-8   PIC S9(12)V9(6) COMP-3.       
049300         15 LETTER-USER-DEFINED-8-TYPE                            
049400                                    PIC X(02).                    
049500         15 LETTER-USER-DEFINED-8-FMT                             
049600                                    PIC 9(02).                    
049700         15 LETTER-USER-DEFINED-9   PIC S9(12)V9(6) COMP-3.       
049800         15 LETTER-USER-DEFINED-9-TYPE                            
049900                                    PIC X(02).                    
050000         15 LETTER-USER-DEFINED-9-FMT                             
050100                                    PIC 9(02).                    
050200         15 LETTER-USER-DEFINED-10  PIC S9(12)V9(6) COMP-3.       
050300         15 LETTER-USER-DEFINED-10-TYPE                           
050400                                    PIC X(02).                    
050500         15 LETTER-USER-DEFINED-10-FMT                            
050600                                    PIC 9(02).                    
050900      10 AMS-ACTG-DATA.                                           
051000         15 BANK-NUM             PIC S9(5) COMP-3.                
051100         15 APP-CODE             PIC S9(3) COMP-3.                
052900      10 ESTIMATED-MONTHLY-PAYMENT  PIC S9(12)V9(6) COMP-3.       
052950      10 STRATA-FIELDS.                                           
052960         15 ACI-INDICATOR        PIC X.                           
052961         15 SERVICING-SYSTEM     PIC X(8).                        
054200      10 GLOBAL-DATA.                                             
054300         15 NATL-CURRENCY        PIC X(03).                       
054400         15 BASE-CURRENCY        PIC X(03).                       
054500         15 PREF-CURRENCY        PIC X(03).                       
055400      10 OTHER-ACCTS   OCCURS    10 TIMES.                        
055500         15 ACCT-TYPE-CODE          PIC X(02).                    
055600         15 LABEL-1-CODE            PIC X(02).                    
               15 STRING-VALUE-1          PIC X(18).                    
055800         15 CURR-VALUE-1 REDEFINES                                
055900               STRING-VALUE-1       PIC S9(12)V9(6) COMP-3.       
056000         15 DATE-VALUE-1 REDEFINES                                
056100               STRING-VALUE-1       PIC 9(8).                     
056200         15 VALUE-1-TYPE-CODE       PIC X(1).                     
056300         15 LABEL-2-CODE            PIC X(02).                    
               15 STRING-VALUE-2          PIC X(18).                    
056500         15 CURR-VALUE-2 REDEFINES                                
056600               STRING-VALUE-2       PIC S9(12)V9(6) COMP-3.       
056700         15 DATE-VALUE-2 REDEFINES                                
056800               STRING-VALUE-2       PIC 9(8).                     
056900         15 VALUE-2-TYPE-CODE       PIC X(1).                     
057000      10 CUSTOMER-SINCE-DATE        PIC 9(06) COMP-3.             
057100      10 VALUE-CODE                 PIC X(02).                    
057200      10 RISK-CODE                  PIC X(02).                    
057300      10 RECOMMENDED-ACTION-CODE    PIC X(02).                    
            10 TIME-ZONE                  PIC X(05).                    
057310      10 ACTG-SYSTEM-ID             PIC X(04).                    
057320      10 ACCOUNT-STATUS-IND.                                      
057330         15 ACCT-STATUS-CODE-1      PIC X(1).                     
057340         15 ACCT-STATUS-CODE-2      PIC X(1).                     
057350         15 ACCT-STATUS-CODE-3      PIC X(1).                     
057351         15 ACCT-STATUS-CODE-4      PIC X(1).                     
057352         15 ACCT-STATUS-CODE-5      PIC X(1).                     
057353         15 ACCT-STATUS-CODE-6      PIC X(1).                     
057354         15 ACCT-STATUS-CODE-7      PIC X(1).                     
057355         15 ACCT-STATUS-CODE-8      PIC X(1).                     
057356         15 ACCT-STATUS-CODE-9      PIC X(1).                     
057357         15 ACCT-STATUS-CODE-10     PIC X(1).                     
057358         15 LEGAL-STATUS-CODE       PIC X(03).                    
057359      10 CAS-SPECIFIC-DATA.                                       
057360         15 RECOVERED-TOTALS.                                     
057361            20  REC-INT-MTD-AMT       PIC S9(12)V9(6) COMP-3.     
057363            20  REC-PRIN-MTD-AMT      PIC S9(12)V9(6) COMP-3.     
057365            20  REC-TOT-COST-MTD-AMT  PIC S9(12)V9(6) COMP-3.     
057370         15 JUDGMENT-DATA.                                        
057371            20 JGMT-EFFECTIVE-DATE    PIC 9(6) COMP-3.            
057380            20 JGMT-INT-AMT           PIC S9(12)V9(6) COMP-3.     
057390            20 JGMT-PRIN-AMT          PIC S9(12)V9(6) COMP-3.     
057392            20 JGMT-TOT-COST-AMT      PIC S9(12)V9(6) COMP-3.     
057394            20 ADDL-JGMTS-IND         PIC X(1).                   
057395         15 RECOVERY-COST-DATA.                                   
057396            20  REC-COSTS-IN-BAL-AMT  PIC S9(12)V9(6) COMP-3.     
057397            20  REC-COSTS-NOT-IN-BAL-AMT                          
057398                                      PIC S9(12)V9(6) COMP-3.     
057399            20  REC-COSTS-ASSESSED-MTD-AMT                        
057400                                      PIC S9(12)V9(6) COMP-3.     
057401            20  REC-COSTS-WAIVED-MTD-AMT                          
057402                                      PIC S9(12)V9(6) COMP-3.     
057403            20  REC-COSTS-CAPTLZED-LTD-AMT                        
057404                                      PIC S9(12)V9(6) COMP-3.     
057403            20  REC-COSTS-BUCKETS.                                
057403                25 REC-COST-1-CAT     PIC X(3).                   
057403                25 REC-COST-1-AMT     PIC S9(12)V9(6) COMP-3.     
057403                25 REC-COST-2-CAT     PIC X(3).                   
057403                25 REC-COST-2-AMT     PIC S9(12)V9(6) COMP-3.     
057403                25 REC-COST-3-CAT     PIC X(3).                   
057403                25 REC-COST-3-AMT     PIC S9(12)V9(6) COMP-3.     
057403                25 REC-COST-4-CAT     PIC X(3).                   
057403                25 REC-COST-4-AMT     PIC S9(12)V9(6) COMP-3.     
057403                25 REC-COST-5-CAT     PIC X(3).                   
057403                25 REC-COST-5-AMT     PIC S9(12)V9(6) COMP-3.     
057403                25 REC-COST-6-CAT     PIC X(3).                   
057403                25 REC-COST-6-AMT     PIC S9(12)V9(6) COMP-3.     
057403                25 REC-COST-7-CAT     PIC X(3).                   
057403                25 REC-COST-7-AMT     PIC S9(12)V9(6) COMP-3.     
057403                25 REC-COST-8-CAT     PIC X(3).                   
057403                25 REC-COST-8-AMT     PIC S9(12)V9(6) COMP-3.     
057403                25 REC-COST-9-CAT     PIC X(3).                   
057403                25 REC-COST-9-AMT     PIC S9(12)V9(6) COMP-3.     
057403                25 REC-COST-10-CAT    PIC X(3).                   
057403                25 REC-COST-10-AMT    PIC S9(12)V9(6) COMP-3.     
057403                25 REC-COST-11-CAT    PIC X(3).                   
057403                25 REC-COST-11-AMT    PIC S9(12)V9(6) COMP-3.     
057403                25 REC-COST-12-CAT    PIC X(3).                   
057403                25 REC-COST-12-AMT    PIC S9(12)V9(6) COMP-3.     
057403                25 REC-COST-13-CAT    PIC X(3).                   
057403                25 REC-COST-13-AMT    PIC S9(12)V9(6) COMP-3.     
057403                25 REC-COST-14-CAT    PIC X(3).                   
057403                25 REC-COST-14-AMT    PIC S9(12)V9(6) COMP-3.     
057403                25 REC-COST-15-CAT    PIC X(3).                   
057403                25 REC-COST-15-AMT    PIC S9(12)V9(6) COMP-3.     
057403                25 REC-COST-16-CAT    PIC X(3).                   
057403                25 REC-COST-16-AMT    PIC S9(12)V9(6) COMP-3.     
057403                25 REC-COST-17-CAT    PIC X(3).                   
057403                25 REC-COST-17-AMT    PIC S9(12)V9(6) COMP-3.     
057403                25 REC-COST-18-CAT    PIC X(3).                   
057403                25 REC-COST-18-AMT    PIC S9(12)V9(6) COMP-3.     
057403                25 REC-COST-19-CAT    PIC X(3).                   
057403                25 REC-COST-19-AMT    PIC S9(12)V9(6) COMP-3.     
057403                25 REC-COST-20-CAT    PIC X(3).                   
057403                25 REC-COST-20-AMT    PIC S9(12)V9(6) COMP-3.     
057407         15 LOAN-POOL-NUM             PIC S9(7) COMP-3.           
057408         15 LOAN-POOL-SOLD-DATE       PIC 9(6) COMP-3.            
057409         15 ORIG-PORTFOLIO-TYPE       PIC X(01).                  
057410         15 NUM-DAYS-SINCE-CHARGE-OFF PIC S9(05) COMP-3.          
057430         15 LAST-EXT-UPD-DATE         PIC 9(6) COMP-3.            
057430         15 LEGAL-STATE               PIC X(3).                   
057430         15 CHARGE-OFF-CHARGE-DOWN-IND PIC 9(1).                  
057430         15 INVESTOR-NUM              PIC S9(7) COMP-3.           
057430      10 EXT-RANDOM-NUM               PIC 9(03).                  
057500      10 FILLER                       PIC X(22).                  
                                                                        
057800                                                                  
