CREATE VIEW RELVW01 (
 PFAPPC01,
 PFAPPC02,
 PFAPPC03,
 PFAPPC04,
 PFAPPC05,
 PFAPPC06,
 PFAPPC07,
 PFAPPC08,
 PFAPPC09,
 PFAPPC10,
 PFAPPC11,
 PFAPPC12,
 PFAPPC13,
 PFAPPC14,
 PFAPPC15,
 PFAPPC16,
 PFAPPC17,
 PFAPPC18)
 AS SELECT
 LF01.PFAPPC,
 LF02.PFAPPC,
 LF03.PFAPPC,
 LF04.PFAPPC,
 LF05.PFAPPC,
 LF06.PFAPPC,
 LF07.PFAPPC,
 LF08.PFAPPC,
 LF09.PFAPPC,
 LF10.PFAPPC,
 LF11.PFAPPC,
 LF12.PFAPPC,
 LF13.PFAPPC,
 LF14.PFAPPC,
 LF15.PFAPPC,
 LF16.PFAPPC,
 LF17.PFAPPC,
 LF18.PFAPPC
 FROM
 RELLF01 LF01
 JOIN RELLF02 LF02
 ON LF01.PFAPPC = LF02.PFAPPC
 JOIN RELLF03 LF03
 ON LF01.PFAPPC = LF03.PFAPPC
 JOIN RELLF04 LF04
 ON LF01.PFAPPC = LF04.PFAPPC
 JOIN RELLF05 LF05
 ON LF01.PFAPPC = LF05.PFAPPC
 JOIN RELLF06 LF06
 ON LF01.PFAPPC = LF06.PFAPPC
 JOIN RELLF07 LF07
 ON LF01.PFAPPC = LF07.PFAPPC
 JOIN RELLF08 LF08
 ON LF01.PFAPPC = LF08.PFAPPC
 JOIN RELLF09 LF09
 ON LF01.PFAPPC = LF09.PFAPPC
 JOIN RELLF10 LF10
 ON LF01.PFAPPC = LF10.PFAPPC
 JOIN RELLF11 LF11
 ON LF01.PFAPPC = LF11.PFAPPC
 JOIN RELLF12 LF12
 ON LF01.PFAPPC = LF12.PFAPPC
 JOIN RELLF13 LF13
 ON LF01.PFAPPC = LF13.PFAPPC
 JOIN RELLF14 LF14
 ON LF01.PFAPPC = LF14.PFAPPC
 JOIN RELLF15 LF15
 ON LF01.PFAPPC = LF15.PFAPPC
 JOIN RELLF16 LF16
 ON LF01.PFAPPC = LF16.PFAPPC
 JOIN RELLF17 LF17
 ON LF01.PFAPPC = LF17.PFAPPC
 JOIN RELLF18 LF18
 ON LF01.PFAPPC = LF18.PFAPPC;

