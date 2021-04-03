*&---------------------------------------------------------------------*
*& Report zbw_trfn_tester
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zbw_trfn_tester.

PARAMETERS: pa_trfn TYPE string.
SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS:
    pa_new RADIOBUTTON GROUP rgr1 USER-COMMAND uc1 DEFAULT 'X',
    pa_ovn RADIOBUTTON GROUP rgr1,
    pa_pte RADIOBUTTON GROUP rgr1.
SELECTION-SCREEN END OF BLOCK b1.


SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002 .
  PARAMETERS:
    pa_stabl RADIOBUTTON GROUP rgr2 MODIF ID g1 USER-COMMAND uc2 DEFAULT 'X',
    pa_sownd RADIOBUTTON GROUP rgr2 MODIF ID g1.

  PARAMETERS: pa_stabn TYPE char15 MODIF ID g12.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN PUSHBUTTON (25) TEXT-003 USER-COMMAND create MODIF ID g13.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b2.


SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-004 .
  PARAMETERS:
    pa_rtabl RADIOBUTTON GROUP rgr3 MODIF ID g3 USER-COMMAND uc3 DEFAULT 'X',
    pa_rownd RADIOBUTTON GROUP rgr3 MODIF ID g3.

  PARAMETERS: pa_rtabn TYPE char15 MODIF ID g14.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN PUSHBUTTON (25) TEXT-005 USER-COMMAND create MODIF ID g15.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-006 .
  PARAMETERS:
    pa_ctrfn TYPE string MODIF ID g2.
SELECTION-SCREEN END OF BLOCK b4.

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.

    IF pa_new = abap_true.

      IF screen-group1 = 'G1'.
        screen-active = '1'.
      ELSEIF screen-group1 = 'G2'.
        screen-active = '0'.
      ENDIF.

      MODIFY SCREEN.
    ENDIF.

    IF pa_ovn = abap_true.

      IF screen-group1 = 'G2'.
        screen-active = '1'.
      ELSEIF screen-group1 = 'G1'
      OR screen-group1 = 'G12'
      OR screen-group1 = 'G13'
      OR screen-group1 = 'G3'
      OR screen-group1 = 'G14'
      OR screen-group1 = 'G15'.
        screen-active = '0'.
      ENDIF.

      MODIFY SCREEN.
    ENDIF.

    IF pa_pte = abap_true.

      IF screen-group1 = 'G4'.
        screen-active = '1'.
      ELSEIF screen-group1 = 'G1'
      OR screen-group1 = 'G12'
      OR screen-group1 = 'G13'
      OR screen-group1 = 'G3'
      OR screen-group1 = 'G14'
      OR screen-group1 = 'G15'.
        screen-active = '0'.
      ENDIF.

      MODIFY SCREEN.
    ENDIF.

  IF pa_stabl = abap_true.

    IF screen-group1 = 'G12'.
      screen-active = '1'.
    ELSEIF screen-group1 = 'G13'.
      screen-active = '0'.
    ENDIF.

    MODIFY SCREEN.
  ENDIF.
  IF pa_sownd = abap_true.

    IF screen-group1 = 'G13'.
      screen-active = '1'.
    ELSEIF screen-group1 = 'G12'.
      screen-active = '0'.
    ENDIF.

    MODIFY SCREEN.
  ENDIF.

  IF pa_rtabl = abap_true.

    IF screen-group1 = 'G14'.
      screen-active = '1'.
    ELSEIF screen-group1 = 'G15'.
      screen-active = '0'.
    ENDIF.

    MODIFY SCREEN.
  ENDIF.

  IF pa_rownd = abap_true.

    IF screen-group1 = 'G15'.
      screen-active = '1'.
    ELSEIF screen-group1 = 'G14'.
      screen-active = '0'.
    ENDIF.

    MODIFY SCREEN.
  ENDIF.

ENDLOOP.
