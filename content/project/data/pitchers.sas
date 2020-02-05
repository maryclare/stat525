Title 'Baseball data: Pitcher File';
 
/*-------------------------------------------------------*
 |                        The Pitcher File               |
 *-------------------------------------------------------*/
 
data PITCHER;
   infile cards missover;
   Length NAME $ 16;
   input name $16. league $1. team $3.
      wins 2.  losses  2. era  5.2 games 3. innings 33-36 saves 37-38
      years 39-40
      winsc 41-43 lossesc 44-46 erac 48-51 gamesc 52-55
      inningc 56-60 savesc 61-63
      salary 64-67 league7 $ 69 team7 $ 70-72;
 
 Label
     NAME    = "Pitcher's name"
     TEAM    = 'Team at the end of 1986'
     LEAGUE  = 'League at the end of 1986'
     WINS    = 'Wins in 1986'
     LOSSES  = 'Losses in 1986'
     ERA     = 'Earned Run Average in 1986'
     GAMES   = 'Games in 1986'
     INNINGS = 'Innings pitched in 1986'
     SAVES   = 'Saves in 1986'
     YEARS   = 'Years in the major leagues'
     WINSC   = 'Wins during his career'
     LOSSESC = 'Losses during his career'
     ERAC    = 'Earned Run Average during his career'
     GAMESC  = 'Games during his career'
     INNINGC = 'Innings pitched during his career'
     SAVESC  = 'Saves during his career'
     SALARY  = '1987 annual salary ($1000s)'
     LEAGUE7 = 'League at the beginning of 1987'
     TEAM7   = 'Team at the beginning of 1987'  ;
datalines;
Don Aase        ABAL 6 7 2.98 6681.234 9 61 54 3.74 325956.2 75 625 ABAL
Jim Acker       NATL 512 4.01 44 155 0 4 20 20 3.99 175411.0 12 350 NATL
Rick Aguilera   NNYN10 7 3.88 28 141 0 2 20 14 3.58  49264.0  0 195 NNYN
Doyle Alexander NATL1110 4.14 34 228 016160135 3.71 467 2709  3   . NATL
Neil Allen      ACHA 7 2 3.82 22 113 0 8 53 58 3.65 367793.2 751200 ACHA
Joaquin Andujar AOAK12 7 3.82 28 155 111122108 3.49 369 2014  91270 AOAK
P Assenmacher   NATL 7 3 2.50 6168.1 7 1  7  3 2.50  61 68.1  7  80 NATL
Keith Atherton  AMIN 610 4.08 6097.010 4 19 28 3.97 202374.0 19 300 AMIN
Scott Bailes    ACLE1010 4.95 62 112 7 1 10 10 4.95  62112.2  7  80 ACLE
Jay Baller      NCHN 2 4 5.37 3653.2 5 3  4  7 4.35  60113.2  6   . NCHN
Scott Bankhead  AKC  8 9 4.61 24 121 0 1  8  9 4.61  24121.0  0  75 ASEA
Floyd Bannister ACHA1014 3.54 28 165 010101117 4.03 300 1832  0 930 ACHA
Steve Bedrosian NPHI 8 6 3.39 6890.129 6 42 45 3.27 294662.1 70 825 NPHI
Eric Bell       ABAL 1 2 5.01  423.1 0 2  1  2 4.97   8 29.0  0  63 ABAL
Mike Bielecki   NPIT 611 4.66 31 148 0 3  8 14 4.53  47198.2  0   . NPIT
Bud Black       AKC  510 3.20 56 121 9 6 46 50 3.71 172834.1  9 600 AKC 
Vida Blue       NSF 1010 3.27 28 156 017209161 3.26 502 3344  2   . NSF 
Bert Blyleven   AMIN1714 4.01 36 271 017229197 3.08 541 3987  01150 AMIN
Mike Boddicker  ABAL1412 4.70 33 218 0 7 63 49 3.60 136900.2  0 800 ABAL
Rich Bordi      ABAL 6 4 4.46 52 107 3 7 17 18 4.00 155330.2 10   . ABAL
Oil.Can Boyd    ABOS1610 3.78 30 214 0 5 47 44 3.86 112791.1  0   . ABOS
Tom Browning    NCIN1413 3.81 39 243 0 3 35 22 3.58  80528.0  0 288 NCIN
Tim Burke       NMON 9 7 2.93 68 101 4 2 18 11 2.64 146221.2 12   . NMON
Ernie Camacho   ACLE 2 4 4.08 5157.120 6  7 16 3.60 138200.0 43 325 ACLE
Bill Campbell   ADET 3 6 3.88 3455.2 314 83 68 3.51 693 1218126  63 NMON
John Candelaria ACAL10 2 2.55 1691.2 012141 89 3.12 350 2016 15 800 ACAL
Tom Candiotti   ACLE1612 3.57 36 252 0 3 22 18 3.68  54340.1  0 160 ACLE
Steve Carlton   ACHA 914 5.10 32 176 022323229 3.11 705 5055  1 150 ACLE
Don Carman      NPHI10 5 3.22 50 134 1 4 19 10 2.91 133235.0  9 245 NPHI
John Cerutti    ATOR 9 4 4.15 34 145 1 2  9  6 4.20  38152.0  1 115 ATOR
Jim Clancy      ATOR1414 3.94 34 219 010102116 4.13 279 1768  0 850 ATOR
Mark Clear      AMIL 5 5 2.20 5973.216 8 62 44 3.80 394689.1 77 625 AMIL
Roger Clemens   ABOS24 4 2.48 33 254 0 3 40 13 3.15  69485.2  0 500 ABOS
Tim Conroy      NSTL 511 5.23 25 115 0 6 15 30 4.61 125426.0  0   . NSTL
Doug Corbett    ACAL 4 2 3.66 4678.210 7 24 28 3.12 302530.0 65   . ACAL
Ed Correa       ATEX1214 4.23 32 202 0 2 13 14 4.36  37212.2  0 115 ATEX
Joe Cowley      ACHA1111 3.88 27 162 0 4 33 21 3.91  90457.2  0 218 NPHI
Danny Cox       NSTL1213 2.90 32 220 0 4 42 39 3.19 108700.1  0 600 NSTL
Ron Darling     NNYN15 6 2.81 34 237 0 4 44 24 3.12 108726.0  01050 NNYN
Danny Darwin    NHOU1110 3.17 39 184 0 9 72 78 3.55 295 1240 17 715 NHOU
Joel Davis      ACHA 4 5 4.70 19 105 0 2  7  8 4.48  31176.2  0  63 ACHA
Mark Davis      NSF  5 7 2.99 6784.1 4 6 22 44 4.36 221534.1 11 415 NSF 
Ron Davis       NCHN 2 8 8.59 5358.2 2 9 46 52 3.94 447692.2130 725 NCHN
Storm Davis     ABAL 912 3.62 25 154 0 5 54 40 3.65 154855.0  1 605 NSD 
Jeff Dedmon     NATL 6 6 2.98 5799.2 3 4 16 12 3.72 176270.2  7 290 NATL
John Denny      NCIN1110 4.20 27 171 013123108 3.58 325 2149  0   . NCIN
Jim Deshaies    NHOU12 5 3.25 26 144 0 3 12  6 3.56  30154.0  0 110 NHOU
Ken Dixon       ABAL1113 4.58 35 202 0 3 19 18 4.17  71377.1  1 155 ABAL
Rich Dotson     ACHA1017 5.48 34 197 0 8 83 76 4.01 206 1294  01000 ACHA
Doug Drabek     ANYA 7 8 4.10 27 131 0 1  7  8 4.10  27131.2  0  85 NPIT
Dave Dravecky   NSD  911 3.07 26 161 0 5 50 43 3.06 169821.1 10 575 NSD 
D Eckersley     NCHN 611 4.57 33 201 012151128 3.67 376 2496  3 783 AOAK
Mark Eichhorn   ATOR14 6 1.72 69 15710 2 14  9 2.45  76195.0 10 165 ATOR
Steve Farr      AKC  8 4 3.13 56 109 8 3 13 16 3.76 103263.0 10 150 AKC 
Sid Fernandez   NNYN16 6 3.52 32 204 1 4 31 22 3.29  75470.2  1 308 NNYN
Mike Flanagan   ABAL 0 0 4.50  1 4.0 0 2  1  1 3.32   4 19.0  0 625 ABAL
Ray Fontenot    AMIN 3 5 5.23 5772.1 2 4 25 26 4.03 143493.2  2   . AMIN
Bob Forsch      NSTL1410 3.25 33 230 013143116 3.62 392 2371  3 750 NSTL
Terry Forster   ACAL 4 1 3.51 4141.0 516 54 65 3.23 614 1105127   . ACAL
John Franco     NCIN 6 6 2.94 74 10129 3 24 11 2.58 195279.1 45 300 NCIN
Gene Garber     NATL 5 5 2.54 6178.02417 88 99 3.29 843 1393194 750 NATL
Scott Garrelts  NSF 13 9 3.11 53 17310 5 26 20 3.18 154360.0 23 290 NSF 
Dwight Gooden   NNYN17 6 2.84 33 250 0 3 58 19 2.28  99744.2  01500 NNYN
Goose Gossage   NSD  5 7 4.45 4564.22115101 89 2.87 725 14822781000 NSD 
Kevin Gross     NPHI1212 4.02 37 241 0 4 39 36 3.79 136672.1  1 420 NPHI
Cecilio Guante  NPIT 5 2 3.35 5278.0 4 5 13 17 3.06 201355.2 20 405 ANYA
Mark Gubicza    AKC 12 6 3.64 35 180 0 3 36 30 3.92  93547.0  0 450 AKC 
Ron Guidry      ANYA 912 3.98 30 192 012163 80 3.25 334 2218  4   . ANYA
Bill Gullickson NCIN1512 3.38 37 244 0 8 87 73 3.43 213 1430  0 900 NCIN
Jose Guzman     ATEX 915 4.54 29 172 0 2 12 17 4.26  34205.0  0 108 ATEX
Greg Harris     ATEX10 8 2.83 73 11120 6 22 25 3.42 216440.0 36 620 ATEX
Andy Hawkins    NSD 10 8 4.30 37 209 0 5 43 37 3.80 142767.1  0 535 NSD 
Neal Heaton     AMIN 715 4.08 33 198 1 5 39 56 4.64 154785.1  8 400 AMIN
Tom Henke       ATOR 9 5 3.35 6391.127 5 15  9 3.34 132191.1 43 291 ATOR
W Hernandez     ADET 8 7 3.55 6488.22410 59 52 3.30 604897.01141060 ADET
Orel Hershiser  NLA 1414 3.85 35 231 0 4 44 25 2.85 124668.2  3 800 NLA 
Joe Hesketh     NMON 6 5 5.01 1582.2 0 3 18 12 3.12  51283.0  1   . NMON
Ted Higuera     AMIL2011 2.79 34 248 0 2 35 19 3.30  66460.2  0 300 AMIL
Rick Honeycutt  NLA 11 9 3.32 32 171 010 87105 3.76 275 1562  1 805 NLA 
Ricky Horton    NSTL 4 3 2.24 42 100 3 3 16  9 2.91 128315.2  5 410 NSTL
Charlie Hough   ATEX1710 3.79 33 230 017131115 3.55 609 2167 61 700 ATEX
Jay Howell      AOAK 3 6 3.38 3853.116 7 26 26 3.97 202390.0 52  95 AOAK
Ken Howell      NLA  612 3.87 6297.212 3 15 24 3.71 150235.0 30 170 NLA 
LaMarr Hoyt     NSD  811 5.15 35 159 0 8 98 68 3.99 244 1311 10   . NSD 
Charles Hudson  NPHI 710 4.94 33 144 0 4 32 42 3.98 127680.0  0 305 ANYA
Mark Huismann   ASEA 3 4 3.79 4697.1 5 4  9  8 3.98 106221.2  8 125 ASEA
Bruce Hurst     ABOS13 8 2.99 25 174 0 7 55 54 4.31 171 1004  0 700 ABOS
Danny Jackson   AKC 1112 3.20 32 185 1 4 28 31 3.54  83488.2  1 225 AKC 
Bob James       ACHA 5 4 5.25 4958.114 7 20 20 3.67 236353.0 63 470 ACHA
Joe Johnson     ATOR13 9 4.42 33 175 0 2 17 13 4.32  48260.2  0 135 ATOR
Barry Jones     NPIT 3 4 2.89 2637.1 3 1  3  4 2.89  26 37.1  3  75 NPIT
Charlie Kerfeld NHOU11 2 2.59 6193.2 7 2 15  4 3.07  72138.0  7 110 NHOU
Jimmy Key       ATOR1411 3.57 36 232 0 3 32 22 3.46 134506.2 10 875 ATOR
Eric King       ADET11 4 3.51 33 138 3 1 11  4 3.51  33138.1  3  80 ADET
Bob Kipper      NPIT 6 8 4.03 20 114 0 2  7 11 4.63  27142.0  0  90 NPIT
Bob Knepper     NHOU1712 3.14 40 258 011114118 3.44 338 2145  11000 NHOU
Mike Krukow     NSF 20 9 3.05 34 245 011108104 3.84 311 1859  1 618 NSF 
Mike LaCoss     NSF 1013 3.57 37 204 0 9 61 67 4.09 281 1179  6 300 NSF 
Pete Ladd       ASEA 8 6 3.82 5270.2 6 6 17 23 4.14 205286.2 39   . ASEA
Dennis Lamp     ATOR 2 6 5.05 4073.0 210 74 76 3.90 396 1353 33   . ATOR
Rick Langford   AOAK 110 7.36 1655.0 011 73106 4.01 260 1490  0   . AOAK
Mark Langston   ASEA1214 4.85 37 239 0 3 36 38 4.43  96591.0  0 370 ASEA
Tim Leary       AMIL1212 4.21 33 188 0 5 17 20 4.09  61288.0  0 170 NLA 
Craig Lefferts  NSD  9 8 3.09 83 107 4 4 22 22 2.89 261385.2 17 500 NSD 
C Leibrandt     AKC 1411 4.09 35 231 0 7 58 44 3.77 173928.1  2 850 AKC 
Dennis Leonard  AKC  813 4.44 33 192 012144106 3.69 312 2187  1   . AKC 
Aurelio Lopez   NHOU 3 3 3.46 4578.0 710 60 35 3.52 433872.1 92 300 NHOU
Ed Lynch        NCHN 7 5 3.73 24 101 0 7 45 45 3.82 190829.1  4 550 NCHN
Mike Maddux     NPHI 3 7 5.42 1678.0 0 1  3  7 5.42  16 78.0  0   . NPHI
Rick Mahler     NATL1418 4.88 39 237 0 8 61 59 3.85 216 1084  2 663 NATL
Mike Mason      ATEX 7 3 4.33 27 135 0 5 25 35 4.31 110532.0  0 280 ATEX
Greg Mathews    NSTL11 8 3.65 23 145 0 1 11  8 3.65  23145.1  0 100 NSTL
Kirk McCaskill  ACAL1710 3.36 34 246 0 2 29 22 3.94  64436.0  0 232 ACAL
Bob McClure     NMON 4 6 3.19 6579.0 612 48 48 3.86 424924.1 41 550 NMON
Lance McCullers NSD 1010 2.78 70 136 5 2 10 12 2.68  91171.0 10 125 NSD 
Roger McDowell  NNYN14 9 3.02 75 12822 2 20 14 2.93 137255.1 39   . NNYN
Andy McGaffigan NMON10 5 2.65 48 142 2 6 20 23 3.44 142455.1  5 293 NMON
Scott McGregor  ABAL1115 4.52 34 203 011136 98 3.84 326 2038  5 950 ABAL
L McWilliams    NPIT 311 5.15 49 122 0 9 68 67 3.91 258 1240  2   . NPIT
Greg Minton     NSF  4 4 3.93 4868.2 512 44 52 3.22 537847.0124 750 NSF 
Dale Mohorcic   ATEX 2 4 2.51 5879.0 7 1  2  4 2.51  58 79.0  7  93 ATEX
Bill Mooneyham  AOAK 4 5 4.52 4599.2 2 1  4  5 4.52  45 99.2  2   . AOAK
Donnie Moore    ACAL 4 5 2.97 4972.22111 36 36 3.64 375596.1 801000 ACAL
Mike Moore      ASEA1113 4.30 38 266 1 5 48 62 4.44 157997.1  1 500 ASEA
Mike Morgan     ASEA1117 4.53 37 216 1 6 21 45 4.91 101507.1  1 170 ASEA
Jack Morris     ADET21 8 3.27 35 267 010144 94 3.58 302 2123  01850 ADET
Jamie Moyer     NCHN 7 4 5.05 1687.1 0 1  7  4 5.05  16 87.1  0  70 NCHN
Gene Nelson     ACHA 6 6 3.85 54 114 6 6 28 34 4.55 160528.2  9 350 AOAK
Tom Niedenfuer  NLA  6 6 3.71 6080.011 6 29 28 2.76 295424.0 63 725 NLA 
Joe Niekro      ANYA 910 4.87 25 125 020213190 3.50 670 3426 16 775 ANYA
Phil Niekro     ACLE1111 4.32 34 210 023311261 3.27 838 5264 30 500 ACLE
Juan Nieves     AMIL1112 4.92 35 184 0 1 11 12 4.92  35184.2  0 115 AMIL
Al Nipper       ABOS1012 5.38 26 159 0 4 31 31 4.35  83519.2  0 325 ABOS
Bob Ojeda       NNYN18 5 2.57 32 217 0 7 62 44 3.83 172935.2  1 625 NNYN
Randy O'Neal    ADET 3 7 4.33 37 122 2 3 10 13 3.82  69235.2  3 100 NATL
Steve Ontiveros AOAK 2 2 4.71 4672.210 2  3  5 3.30  85147.1 18   . AOAK
Jesse Orosco    NNYN 8 6 2.33 5881.021 7 44 38 2.48 314518.1 91 925 NNYN
David Palmer    NATL1110 3.65 35 209 0 7 49 36 3.36 157787.1  2 725 NATL
Dan Petry       ADET 510 4.66 20 116 0 8 98 74 3.58 227 1504  0 975 ADET
Dan Plesac      AMIL10 7 2.97 5191.014 1 10  7 2.97  51 91.0 14 140 AMIL
Eric Plunk      AOAK 4 7 5.31 26 120 0 1  4  7 5.31  26120.1  0  75 AOAK
Mark Portugal   AMIN 610 4.31 27 112 1 2  7 13 4.53  33137.0  1  85 AMIN
Ted Power       NCIN10 6 3.70 56 129 1 6 34 29 3.72 264476.1 41 500 NCIN
Dan Quisenberry AKC  3 7 2.77 6281.112 8 47 42 2.51 506845.1229 875 AKC 
D Rasmussen     ANYA18 6 3.88 31 202 0 4 30 17 4.06  81465.0  0 175 ANYA
Shane Rawley    NPHI11 7 3.54 23 157 0 9 81 79 3.80 374 1298 40 800 NPHI
Jeff Reardon    NMON 7 9 3.94 6289.035 8 42 46 2.80 456666.0162 825 AMIN
Rick Reuschel   NPIT 916 3.96 35 215 014162155 3.43 436 2771  4 750 NPIT
Rick Rhoden     NPIT1512 2.84 34 253 013121 97 3.48 333 2118  1 575 ANYA
Dave Righetti   ANYA 8 8 2.45 74 10646 7 58 44 3.01 294832.0107 800 ANYA
Jose Rijo       AOAK 911 4.65 39 193 1 3 17 23 4.45  75319.2  3 110 AOAK
Don Robinson    NPIT 3 4 3.38 5069.114 9 59 63 3.85 301 1137 31 620 NPIT
Jeff Robinson   NSF  6 3 3.36 64 104 8 3 13 18 4.15 106288.1  8 190 NSF 
Ron Romanick    ACAL 5 8 5.50 18 106 0 3 31 29 4.24  82531.0  0   . ACAL
Bruce Ruffin    NPHI 9 4 2.46 21 146 0 1  9  4 2.46  21146.1  0 100 NPHI
Nolan Ryan      NHOU12 8 3.34 30 178 020253226 3.15 611 4115  31000 NHOU
Bret Saberhagen AKC  712 4.15 30 156 0 3 37 29 3.41 100549.0  1 740 AKC 
Joe Sambito     ABOS 2 0 4.84 5344.21210 35 32 2.79 414590.2 84 360 ABOS
Scott Sanderson NCHN 911 4.19 37 169 1 9 78 69 3.40 229 1313  3 867 NCHN
C Schiraldi     ABOS 4 2 1.41 2551.0 9 3  6  5 4.28  40 94.2  9 150 ABOS
Dave Schmidt    ACHA 3 6 3.31 4992.1 8 6 23 28 3.18 221436.1 34 375 ABAL
Ken Schrom      ACLE14 7 4.54 34 206 0 6 45 38 4.46 144746.1  1 425 ACLE
Mike Scott      NHOU1810 2.22 37 275 0 8 65 62 3.70 212 1160  3 667 NHOU
Tom Seaver      ABOS 713 4.03 28 176 020311205 2.86 656 4782  1   . ABOS
Bob Sebra       NMON 5 5 3.55 1791.1 0 2  5  7 4.27  24111.2  0  70 NMON
Jeff Sellers    ABOS 3 7 4.94 1482.0 0 2  5  7 4.66  18104.1  0  78 ABOS
Bob Shirley     ANYA 0 4 5.04 39 105 310 66 94 3.75 419 1390 18 500 ANYA
Eric Show       NSD  9 5 2.97 24 136 0 6 62 46 3.30 188949.2  6 788 NSD 
Jim Slaton      ADET 4 6 5.08 36 113 216151158 4.03 496 2683 14 400 AOAK
Bryn Smith      NMON10 8 3.94 30 187 0 6 49 41 3.27 193836.1  6   . NMON
Dave Smith      NHOU 4 7 2.73 5456.033 7 38 29 2.61 361526.2100 650 NHOU
Lee Smith       NCHN 9 9 3.09 6690.131 7 36 41 2.89 396598.1144 820 NCHN
Zane Smith      NATL 816 4.05 38 204 1 3 18 26 3.85  83371.2  1 150 NATL
Mike Smithson   AMIN1314 4.77 34 198 0 5 56 59 4.19 148977.0  0 540 AMIN
Mario Soto      NCIN 510 4.71 19 105 010 94 83 3.37 277 1611  41200 NCIN
Bob Stanley     ABOS 6 6 4.37 6682.11610100 76 3.45 503 13741231075 ABOS
Dave Stewart    AOAK 9 5 3.95 37 161 0 7 39 40 3.96 247766.0 19 500 AOAK
Dave Stieb      ATOR 712 4.74 37 205 1 8102 92 3.34 259 1859  1 850 ATOR
Rick Sutcliffe  NCHN 514 4.64 28 176 010 86 68 3.84 251 1416  61800 NCHN
Don Sutton      ACAL1511 3.74 34 207 021310239 3.20 723 5001  5 550 ACAL
Bill Swift      ASEA 2 9 5.46 29 115 0 2  8 19 5.11  52236.0  0   . ASEA
Frank Tanana    ADET12 9 4.16 32 188 014159153 3.40 408 2759  0 663 ADET
Kent Tekulve    NPHI11 5 2.54 73 110 413 85 76 2.69 853 1199176 800 NPHI
Walt Terell     ADET1512 4.56 34 217 0 5 49 45 3.89 125816.0  0 600 ADET
Bob Tewksbury   ANYA 9 5 3.31 23 130 0 1  9  5 3.31  23130.1  0  85 ANYA
Mark Thurmond   ADET 7 8 4.56 42 122 3 4 35 30 3.50 131554.2  5 370 ADET
Jay Tibbs       NMON 7 9 3.97 35 190 0 3 23 27 3.73  84509.0  0 160 NMON
Steve Trout     NCHN 5 7 4.75 37 161 0 9 74 75 3.94 242 1294  4 890 NCHN
John Tudor      NSTL13 7 2.92 30 219 0 8 85 58 3.26 204 1342  11050 NSTL
Frank Viola     AMIN1613 4.51 37 245 0 5 63 64 4.38 165 1090  0 830 AMIN
Bob Walk        NPIT 7 8 3.75 44 141 2 7 33 32 4.35 127573.2  2 285 NPIT
Bill Wegman     AMIL 512 5.13 35 198 0 2  7 12 5.00  38216.0  0  80 AMIL
Chris Welsh     NCIN 6 9 4.78 24 139 0 5 22 31 4.45 122538.0  0   . NCIN
Mitch Williams  ATEX 8 6 3.58 8098.0 8 1  8  6 3.58  80 98.0  8 120 ATEX
Frank Wills     ACLE 4 4 4.91 2640.1 4 4 13 19 5.40  66235.0  5   . ACLE
Jim Winn        NPIT 3 5 3.58 5088.0 3 4  7 11 4.47  96193.1  4 140 ACHA
Bobby Witt      ATEX11 9 5.48 31 157 0 1 11  9 5.48  31157.2  0  93 ATEX
Todd Worrell    NSTL 910 2.08 74 10336 2 12 10 2.23  91125.1 41 150 NSTL
Floyd Youmans   NMON1312 3.53 33 219 0 2 17 15 3.25  47296.0  0 145 NMON
Curt Young      AOAK13 9 3.45 29 198 0 4 22 18 4.43  76361.2  0 150 AOAK
Matt Young      ASEA 8 6 3.82 65 10313 4 37 48 4.35 157639.0 14 350 NLA 
Ron Robinson    NCIN10 3 3.24 70 11614 3 18 12 3.47 115264.2 15 185 NCIN
F Valenzuela    NLA 2111 3.14 34 269 0 7 99 68 2.94 210 1554  11850 NLA 
Bob Welch       NLA  713 3.28 33 235 0 9100 77 3.31 257 1568  8 838 NLA 
Brian Fisher    ANYA 9 5 4.93 6296.2 6 2 13  9 3.65 117195.0 20 180 NPIT
Chris Codiroli  AOAK 5 8 4.03 1691.2 0 5 38 40 4.59 121629.1  2 295 AOAK
;
run;

proc print data=PITCHER;
run;




