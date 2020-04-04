$regfile = "m128def.dat"
$crystal = 11059200
'$crystal = 8000000
$baud = 19200

$hwstack = 64
$swstack = 128
$framesize = 288

Config Porta = Output
Config Porta.5 = Input
Config Porta.6 = Input
Config Portb = Output
Config Portd = Output
Config Portf = Input
Config Portg = Input
Config Portc.0 = Input

Config Porte.7 = Output
Config Porte.6 = Output


Config Spi = Hard , Interrupt = Off , Data Order = Msb , Master = Yes , Polarity = Low , Phase = 0 , Clockrate = 16
Spiinit

Config Timer0 = Timer , Prescale = 1024
Config Timer1 = Timer , Prescale = 1
Config Timer2 = Timer , Prescale = 1024

On Urxc Srin2
On Ovf0 T0_postscale
On Ovf1 T1_postscale
On Ovf2 T2_postscale


Config Watchdog = 2048
Start Watchdog


Dim X As Byte , Y As Byte , I As Byte , J As Byte , K As Byte , S As String * 1 , S1 As String * 50 , S2 As String * 15
Dim A As Word , B(20) As Word , C As Long , D(2) As String * 50 , G(6) As String * 10 , H(10) As Byte
Dim Aa As Word , Bb As Word

Dim M As Long , O As Long , P As Long
Dim E As Single , F As Long , Z As Integer


Dim Adcheck As Word
Dim Sum As Long , Sum_temp As Long , Chcksum As Byte

Dim Sok As Bit , Initz As Byte


Dim T0_p As Byte , T1_p As Byte , T2_p As Byte
Dim T0_def As Byte , T1_def As Byte , T2_def As Byte


Dim Is_diag As Byte , Is_service As Byte

Dim Adc_rd(11) As Word

Dim X1_co As Long
Dim X2_co As Long
Dim Y1_co As Long
Dim Y2_co As Long

Dim Gant_set As Long
Dim Collim_set As Long

Dim X1_set As Long
Dim X2_set As Long
Dim Y1_set As Long
Dim Y2_set As Long


Dim X1_tol As Long , X1_tol0 As Long , X1_tol1 As Long , X1_tol2 As Long
Dim X2_tol As Long , X2_tol0 As Long , X2_tol1 As Long , X2_tol2 As Long
Dim Y1_tol As Long , Y1_tol0 As Long , Y1_tol1 As Long , Y1_tol2 As Long
Dim Y2_tol As Long , Y2_tol0 As Long , Y2_tol1 As Long , Y2_tol2 As Long

Dim X1_v As Byte , X1_v1 As Byte , X1_v2 As Byte , X1_v3 As Byte
Dim X2_v As Byte , X2_v1 As Byte , X2_v2 As Byte , X2_v3 As Byte
Dim Y1_v As Byte , Y1_v1 As Byte , Y1_v2 As Byte , Y1_v3 As Byte
Dim Y2_v As Byte , Y2_v1 As Byte , Y2_v2 As Byte , Y2_v3 As Byte

Dim Gant_co As Word
Dim Gant_f1 As Word
Dim Gant_f2 As Word
Dim Gant_cofin As Long


Dim Collim_co As Word
Dim Collim_f1 As Word
Dim Collim_f2 As Word
Dim Collim_cofin As Long

Dim Gant_zpnt As Word , Gant_length As Long , Gant_fine_length As Word , Gant_gain As Long , Gant_offset As Long
Dim Collim_zpnt As Word , Collim_length As Long , Collim_fine_length As Word , Collim_gain As Long , Collim_offset As Long


Dim Gant As Long , Gant_tol As Long , Gant_tol0 As Long , Gant_tol1 As Long , Gant_tol2 As Long
Dim Collim As Long , Collim_tol As Long , Collim_tol0 As Long , Collim_tol1 As Long , Collim_tol2 As Long

Dim Gant_sin As Single , Collim_sin As Single , Collim_cos As Single , Gant_angle As Single , Collim_angle As Single
Dim X_gravity_comp As Single , Y_gravity_comp As Single


Dim Gravity_up As Byte

Dim X_jaws_c As Word , Y_jaws_c As Word
Dim X1_in_end_c As Word , X1_out_end_c As Word
Dim X2_in_end_c As Word , X2_out_end_c As Word
Dim Y1_in_end_c As Word , Y1_out_end_c As Word
Dim Y2_in_end_c As Word , Y2_out_end_c As Word

Dim X1_tt As Byte
Dim X2_tt As Byte
Dim Y1_tt As Byte
Dim Y2_tt As Byte

Const Gant_lower_limit = 1024
Dim Gant_upper_limit As Word

Const Collim_lower_limit = 1024
Dim Collim_upper_limit As Word

Dim Gant_v As Byte , Gant_v1 As Byte , Gant_v2 As Byte , Gant_v3 As Byte
Dim Collim_v As Byte , Collim_v1 As Byte , Collim_v2 As Byte , Collim_v3 As Byte




Dim Count As Byte
Dim R_counter As Long , P_counter As Long , S_counter As Long

R_counter = 0
P_counter = 0
S_counter = 0

X1_tt = 0
X2_tt = 0
Y1_tt = 0
Y2_tt = 0

Const Send_delay = 20

Gant_fr Alias Portd.0
Gant_rr Alias Portd.1
Collim_on Alias Portd.2
X1_on Alias Portd.3
X2_on Alias Portd.4
Y1_on Alias Portd.5
Y2_on Alias Portd.6

Dac_ldac Alias Portb.6
Dac_sync Alias Portb.4

Adc_rc Alias Porta.4
Adc_sts Alias Pinc.0


Declare Sub Ao(byval M As Byte , Byval N As Byte)
Declare Function F_v(f_set As Long , F_co As Long , F_f As Long , F_tol1 As Long , F_tol2 As Long , F_v1 As Byte , F_v2 As Byte , F_v3 As Byte) As Byte
Declare Function Get_init() As Long
Declare Sub Send(byval S As String * 3 , Byval A As Long)
Declare Sub Get_setpoint()
Declare Function Get_diag() As Byte
Declare Function Adc_read(byval Adc_number As Byte , Byval Adc_repeat As Byte) As Word

Dim Chk As Byte

Declare Function X1_check() As Byte
Declare Function X2_check() As Byte
Declare Function Y1_check() As Byte
Declare Function Y2_check() As Byte

Reset Watchdog
Enable Interrupts

Portb.7 = 1

Collim_on = 1
X1_on = 1
X2_on = 1
Y1_on = 1
Y2_on = 1

Call Ao(0 , 0)
Call Ao(128 , 1)
Call Ao(128 , 2)
Call Ao(128 , 3)
Call Ao(128 , 4)
Call Ao(128 , 5)

T0_def = 253
T1_def = 247
T2_def = 236

T0_p = T0_def
T1_p = T1_def
T2_p = T2_def

Is_diag = 100
Is_service = 100

Initz = 0
Print "ini"

X = Waitkey()
If X = 122 Then Gosub Init

Do
Loop Until Initz = 164

Timer1 = 30000

Start Timer0
Start Timer1
Start Timer2



Enable Urxc
Enable Ovf0
Enable Ovf1
Enable Ovf2


'Gosub Read_positions


Do

Loop



End                                                         'end program


Init:

   Reset Watchdog
   Initz = 0
   Sum = 0

   Gant_zpnt = Get_init()
   Gant_length = Get_init()
   Gant_fine_length = Get_init()
   Gant_gain = Get_init()
   Gant_offset = Get_init()
   Collim_zpnt = Get_init()
   Collim_length = Get_init()
   Collim_fine_length = Get_init()
   Collim_gain = Get_init()
   Collim_offset = Get_init()
   Gant_tol = Get_init()
   Gant_tol0 = Get_init()
   Gant_tol1 = Get_init()
   Gant_tol2 = Get_init()
   Gant_v1 = Get_init()
   Gant_v2 = Get_init()
   Gant_v3 = Get_init()
   Collim_tol = Get_init()
   Collim_tol0 = Get_init()
   Collim_tol1 = Get_init()
   Collim_tol2 = Get_init()
   Collim_v1 = Get_init()
   Collim_v2 = Get_init()
   Collim_v3 = Get_init()
   X1_tol = Get_init()
   X1_tol0 = Get_init()
   X1_tol1 = Get_init()
   X1_tol2 = Get_init()
   X1_v1 = Get_init()
   X1_v2 = Get_init()
   X1_v3 = Get_init()
   X2_tol = Get_init()
   X2_tol0 = Get_init()
   X2_tol1 = Get_init()
   X2_tol2 = Get_init()
   X2_v1 = Get_init()
   X2_v2 = Get_init()
   X2_v3 = Get_init()
   Y1_tol = Get_init()
   Y1_tol0 = Get_init()
   Y1_tol1 = Get_init()
   Y1_tol2 = Get_init()
   Y1_v1 = Get_init()
   Y1_v2 = Get_init()
   Y1_v3 = Get_init()
   Y2_tol = Get_init()
   Y2_tol0 = Get_init()
   Y2_tol1 = Get_init()
   Y2_tol2 = Get_init()
   Y2_v1 = Get_init()
   Y2_v2 = Get_init()
   Y2_v3 = Get_init()
   Gravity_up = Get_init()
   Y_jaws_c = Get_init()
   X_jaws_c = Get_init()
   Y1_in_end_c = Get_init()
   Y1_out_end_c = Get_init()
   Y2_in_end_c = Get_init()
   Y2_out_end_c = Get_init()
   X1_in_end_c = Get_init()
   X1_out_end_c = Get_init()
   X2_in_end_c = Get_init()
   X2_out_end_c = Get_init()

   Waitms 1
   S1 = Str(sum)
   Print "sum" ; Sum

   X = Waitkey()

   If X = 123 Then
      X = waitkey()
      If X = 124 Then
         X = waitkey()
         If X = 125 Then
            X = waitkey()
            If X = 126 Then Initz = 164
         End If
      End If
   End If

Return

Function Get_init()
   S1 = ""
   Do
      X = Waitkey()
      If X = 47 Then Exit Do
      S1 = S1 + Chr(x)
   Loop
   Get_init = Val(s1)
   Sum = Sum + Get_init
End Function


Srin2:
   Reset Watchdog
   Disable Ovf0
   Disable Ovf1
   Disable Ovf2

   S = Waitkey()

   If Is_service = 200 Then
      If Is_diag = 200 Then
         Select Case S
            Case "@":
               Print "dia"
               S = Waitkey()
               Select Case S
                  Case "a":
                     P = 0
                     P = Get_diag()
                     'print "gant" ; P
                     P = P - 128
                     P = P * 2
                     If P > 0 Then
                        Gant_rr = 0
                        Gant_fr = 1
                     Elseif P < 0 Then
                        Gant_rr = 1
                        Gant_fr = 0
                     Else
                        Gant_rr = 0
                        Gant_fr = 0
                     End If
                     Gant_v = Abs(p)
                     Call Ao(gant_v , 0)

                  Case "b":
                     Collim_v = 0
                     Collim_v = Get_diag()
                     'print "col" ; Collim_v
                     Call Ao(collim_v , 1)
                     If Collim_v = 128 Then
                        Collim_on = 1
                     Else
                        Collim_on = 0
                     End If

                  Case "c":
                     X1_v = 0
                     X1_v = Get_diag()
                     'Print "x1" ; X1_v
                     Call Ao(x1_v , 2)
                     If X1_v = 128 Then
                        X1_on = 1
                     Else
                        X1_on = 0
                     End If

                  Case "d":
                  X2_v = 0
                     X2_v = Get_diag()
                     'Print "x2" ; X2_v
                     Call Ao(x2_v , 3)
                     If X2_v = 128 Then
                        X2_on = 1
                     Else
                        X2_on = 0
                     End If

                  Case "e":
                     Y1_v = 0
                     Y1_v = Get_diag()
                     'Print "y1" ; Y1_v
                     Call Ao(y1_v , 4)
                     If Y1_v = 128 Then
                        Y1_on = 1
                     Else
                        Y1_on = 0
                     End If

                  Case "f":
                     Y2_v = 0
                     Y2_v = Get_diag()
                     'Print "y2" ; Y2_v
                     Call Ao(y2_v , 5)
                     If Y2_v = 128 Then
                        Y2_on = 1
                     Else
                        Y2_on = 0
                     End If
               End Select

            Case "s":
              Is_diag = 100

            Case "x":
               Is_service = 100
               Is_diag = 100

            Case "w":
               Do
               Loop

         End Select
      Else
         Select Case S
            Case "S":
               X = Waitkey()
               If X = 36 Then
                  Is_diag = 200
                  Gant_fr = 0
                  Gant_rr = 0
                  Collim_on = 1
                  X1_on = 1
                  X2_on = 1
                  Y1_on = 1
                  Y2_on = 1
                  Gant_v = 0
                  Collim_v = 128
                  X1_v = 128
                  X2_v = 128
                  Y1_v = 128
                  Y2_v = 128

                  Call Ao(gant_v , 0)
                  Call Ao(collim_v , 1)
                  Call Ao(x1_v , 2)
                  Call Ao(x2_v , 3)
                  Call Ao(y1_v , 4)
                  Call Ao(y2_v , 5)
               End If

            Case "t":
               Gosub Gant_learn

            Case "u":
               Gosub Collim_learn

            Case "w":
               Do
               Loop

            Case "x":
               Is_service = 100
               Is_diag = 100

            Case "$":
               Call Get_setpoint()


        End Select
      End If
   Else
      Select Case S
         Case "$" :
            Call Get_setpoint()

         Case "y" :
            Is_service = 200

         Case "s":
              Is_diag = 100

         Case "w":
            Do
            Loop

      End Select
   End If

   If Is_diag = 100 Or Is_service = 100 Then Enable Ovf0
   Enable Ovf1
   Enable Ovf2


Return


Sub Get_setpoint()

   S1 = ""
   Print "req"
   Do
      X = Waitkey()
      If X = 38 Then Exit Do
      S1 = S1 + Chr(x)
   Loop
   Print "&&&"

   A = Split(s1 , D(1) , "c")
   Chcksum = Val(d(2))
   If Checksum(d(1)) = Chcksum And A = 2 Then
      A = Split(d(1) , G(1) , "/")
      For I = 1 To A
         S = Left(g(i) , 1)
         Delchar G(i) , 1
         Select Case S
            Case "m":
               Gant_set = Val(g(i))
            Case "n":
               Collim_set = Val(g(i))
            Case "o":
               X1_set = Val(g(i))
            Case "p":
               X2_set = Val(g(i))
            Case "q":
               Y1_set = Val(g(i))
            Case "r":
               Y2_set = Val(g(i))

         End Select
      Next
   End If

End Sub

Function Get_diag()
   S2 = ""
   Do
      X = Waitkey()
      If X = 47 Then Exit Do
      S2 = S2 + Chr(x)
   Loop
   Get_diag = Val(s2) + 128
End Function

Srout:
   'Incr S_counter
   Reset Watchdog

   If Is_service = 200 Then
      Call Send( "gco" , Gant_co)
      Call Send( "gf1" , Gant_f1)
      Call Send( "gf2" , Gant_f2)
   End If
   Call Send( "gfn" , Gant_cofin)

   If Is_service = 200 Then
      Call Send( "cco" , Collim_co)
      Call Send( "cf1" , Collim_f1)
      Call Send( "cf2" , Collim_f2)
   End If

   Call Send( "cfn" , Collim_cofin)

   Call Send( "wco" , X1_co)
   Call Send( "xco" , X2_co)
   Call Send( "yco" , Y1_co)
   Call Send( "zco" , Y2_co)

   Call Send( "gnd" , Gant_set)
   Call Send( "cld" , Collim_set)
   Call Send( "x1d" , X1_set)
   Call Send( "x2d" , X2_set)
   Call Send( "y1d" , Y1_set)
   Call Send( "y2d" , Y2_set)

   If Is_diag = 200 Then Print "SSS"
   Waitus Send_delay

   If Is_service = 100 Then
      Print "ccc"
      Waitus Send_delay

   Elseif Is_service = 200 Then
      Print "sss"
      Waitus Send_delay

   End If

   Call Send( "adc" , Adcheck)

   If X1_tt > 0 Then Decr X1_tt
   If X2_tt > 0 Then Decr X2_tt
   If Y1_tt > 0 Then Decr Y1_tt
   If Y2_tt > 0 Then Decr Y2_tt
   '(
   Gant_cofin = Gant_cofin + 10
   Collim_cofin = Collim_cofin + 5
   X1_co = X1_co + 2
   X2_co = X2_co + 2
   Y1_co = Y1_co + 2
   Y2_co = Y2_co + 2
   ')
Return

Sub Send(byval S As String * 3 , Byval A As Long)

   Local S1 As String * 15

   S1 = Str(a)
   Print S ; S1
   Waitus Send_delay

End Sub



Read_positions:
   'Incr R_counter
   Reset Watchdog

   For I = 1 To 11
      Adc_rd(i) = 0
      Adc_rd(i) = Adc_read(i , 20)
   Next I

   Gant_co = Adc_rd(1)
   Gant_f1 = Adc_rd(2)
   Gant_f2 = Adc_rd(3)

   Collim_co = Adc_rd(4)
   Collim_f1 = Adc_rd(5)
   Collim_f2 = Adc_rd(6)

   X1_co = Adc_rd(7)
   X2_co = Adc_rd(8)
   Y1_co = Adc_rd(9)
   Y2_co = Adc_rd(10)

   Adcheck = Adc_rd(11)

   e = Gant_co - Gant_zpnt
   e = e / Gant_length
   F = Int(e)
   I = F Mod 2
   If I = 0 Then
         E = Gant_f1 - Gant_lower_limit
   Else
         E = Gant_f2 - Gant_lower_limit
   End If
   F = F * Gant_fine_length
   Gant_cofin = E + F

   E = Collim_co - Collim_zpnt
   E = E / Collim_length
   F = Int(e)
   I = F Mod 2
   If I = 0 Then
      E = Collim_f1 - Collim_lower_limit
   Else
      E = Collim_f2 - Collim_lower_limit
   End If
   F = F * Collim_fine_length
   Collim_cofin = E + F


   Gant_cofin = -58618
   Collim_cofin = 28017
   X1_co = 2048
   X2_co = 2048
   Y1_co = 2048
   Y2_co = 2048

   Adcheck = 2048

Return

Function Adc_read(byval Adc_number As Byte , Byval Adc_repeat As Byte) As Word
   Y = 32 - Adc_number
   Porta = Y
   Waitus 1

   For J = 1 To Adc_repeat
      Adc_rc = 0
      Waitus 1
      Do
      Loop Until Adc_sts = 0
      Adc_rc = 1
      Waitus 1
      B(j) = 0
      B(j) = Pinf
      A = 0
      A = Ping
      A = A And 15
      A = A * 256
      B(j) = B(j) + A
   Next J

   If Adc_repeat > 3 Then Sort B(1) , Adc_repeat

   C = 0
   K = 0

   X = Adc_repeat / 4

   Y = Adc_repeat - X

   X = X + 1

   For J = X To Y
      C = C + B(j)
      Incr K
   Next J

   Adc_read = C / K
End Function



Positioning:
   'Incr P_counter
   Reset Watchdog


   Gant = Gant_cofin / 10
   Gant = Gant * Gant_gain
   Gant = Gant / 10000
   Gant = Gant + Gant_offset
   Gant_angle = Gant
   Gant_angle = Deg2rad(gant_angle)

   Collim = Collim_cofin / 100
   Collim = Collim * Collim_gain
   Collim = Collim / 1000
   Collim = Collim + Collim_offset
   Collim_angle = Collim
   Collim_angle = Deg2rad(collim_angle)



   Gant_sin = Sin(gant_angle)
   Collim_sin = Sin(collim_angle)
   Collim_cos = Cos(collim_angle)

   X_gravity_comp = Gravity_up * Gant_sin
   X_gravity_comp = X_gravity_comp / 100

   Y_gravity_comp = X_gravity_comp * Collim_cos
   X_gravity_comp = X_gravity_comp * Collim_sin


   If Gant_set <> 0 Then

      If Gant_rr = 0 And Gant_fr = 0 Then
         F = Gant_tol0
      Else
         F = Gant_tol
      End If

      If Gant_set > Gant_cofin Then
         O = Gant_set - Gant_cofin
         If O > F Then

            Gant_rr = 0
            Gant_fr = 1

            If O < Gant_tol1 Then Gant_v = Gant_v1
            If Gant_tol1 <= O And O < Gant_tol2 Then Gant_v = Gant_v2
            If Gant_tol2 < O Then Gant_v = Gant_v3
            Gant_v = Gant_v * 2
         Else
            Gant_rr = 0
            Gant_fr = 0
            Gant_v = 0
         End If

      Elseif Gant_set < Gant_cofin Then
         O = Gant_cofin - Gant_set
         If O > F Then

            Gant_fr = 0
            Gant_rr = 1

            If O < Gant_tol1 Then Gant_v = Gant_v1
            If Gant_tol1 <= O And O < Gant_tol2 Then Gant_v = Gant_v2
            If Gant_tol2 < O Then Gant_v = Gant_v3
            Gant_v = Gant_v * 2
         Else
            Gant_fr = 0
            Gant_rr = 0
            Gant_v = 0
         End If
      Else
         Gant_fr = 0
         Gant_rr = 0
         Gant_v = 0
      End If
   Else
      Gant_fr = 0
      Gant_rr = 0
      Gant_v = 0
   End If
   Call Ao(gant_v , 0)


   If Collim_set <> 0 Then

      If Collim_on = 1 Then
         F = Collim_tol0
      Else
         F = Collim_tol
      End If

      O = 0
      O = F_v(collim_set , Collim_cofin , F , Collim_tol1 , Collim_tol2 , Collim_v1 , Collim_v2 , Collim_v3)
      Collim_v = 256 - o

   Else
      Collim_v = 128
   End If


   If X1_set <> 0 Then

      If X1_on = 1 Then
         F = X1_tol0
      Else
         F = X1_tol
      End If


      X1_v = 0
      X1_v = F_v(x1_set , X1_co , F , X1_tol1 , X1_tol2 , X1_v1 , X1_v2 , X1_v3)

      If X1_v <> 128 Then
         M = 128 - X1_v
         M = Abs(m)
         If M < X1_v3 Then
            E = M * X_gravity_comp
            M = Round(e)
            X1_v = X1_v - M
         End If
      End If

   Else
      X1_v = 128
   End If


   If X2_set <> 0 Then

      If X2_on = 1 Then
         F = X2_tol0
      Else
         F = X2_tol
      End If

      X2_v = 0
      X2_v = F_v(x2_set , X2_co , F , X2_tol1 , X2_tol2 , X2_v1 , X2_v2 , X2_v3)

      If X2_v <> 128 Then
         M = 128 - X2_v
         M = Abs(m)
         If M < X2_v3 Then
            E = M * X_gravity_comp
            M = Round(e)
            X2_v = X2_v + M
         End If
      End If
   Else
      X2_v = 128
   End If


   If Y1_set <> 0 Then

      If Y1_on = 1 Then
         F = Y1_tol0
      Else
         F = Y1_tol
      End If

      Y1_v = 0
      Y1_v = F_v(y1_set , Y1_co , F , Y1_tol1 , Y1_tol2 , Y1_v1 , Y1_v2 , Y1_v3)

      If Y1_v <> 128 Then
         M = 128 - Y1_v
         M = Abs(m)
         If M < Y1_v3 Then
            E = M * Y_gravity_comp
            M = Round(e)
            Y1_v = Y1_v + M
         End If
      End If

   Else
      Y1_v = 128
   End If


   If Y2_set <> 0 Then

      If Y2_on = 1 Then
         F = Y2_tol0
      Else
         F = Y2_tol
      End If

      Y2_v = 0
      Y2_v = F_v(y2_set , Y2_co , F , Y2_tol1 , Y2_tol2 , Y2_v1 , Y2_v2 , Y2_v3)

      If Y2_v <> 128 Then
         M = 128 - Y2_v
         M = Abs(m)
         If M < Y2_v3 Then
            E = M * Y_gravity_comp
            M = Round(e)
            Y2_v = Y2_v - M
         End If
      End If
   Else
      Y2_v = 128
   End If


   Call Ao(collim_v , 1)
   Call Ao(x1_v , 2)
   Call Ao(x2_v , 3)
   Call Ao(y1_v , 4)
   Call Ao(y2_v , 5)

   If Collim_set = 0 Then
      Collim_on = 1
   Else
      Collim_on = 0
   End If

   If X1_set = 0 Then
      X1_on = 1
   Else
      If X1_check() = 1 Then
         X1_on = 0
      Else
         X1_on = 1
      End If
   End If

   If x2_set = 0 Then
      x2_on = 1
   Else
      If X2_check() = 1 Then
         X2_on = 0
      Else
         X2_on = 1
      End If
   End If

   If y1_set = 0 Then
      y1_on = 1
   Else
      If y1_check() = 1 Then
         Y1_on = 0
      Else
         Y2_on = 1
      End If
   End If

   If y2_set = 0 Then
      Y2_on = 1
   Else
      If Y2_check() = 1 Then
         Y2_on = 0
      Else
         Y2_on = 1
      End If
   End If


Return


Function F_v(f_set As Long , F_co As Long , F_f As Long , F_tol1 As Long , F_tol2 As Long , F_v1 As Byte , F_v2 As Byte , F_v3 As Byte) As Byte

   If F_set > F_co Then
      O = F_set - F_co
      If F_f < O Then
         If O < F_tol1 Then F_v = F_v1
         If F_tol1 <= O And O < F_tol2 Then F_v = F_v2
         If F_tol2 < O Then F_v = F_v3
         F_v = 128 - F_v
      Else
         F_v = 128

      End If

   Elseif F_set < F_co Then
      O = F_co - F_set
      If F_f < O Then
         If O < F_tol1 Then F_v = F_v1
         If F_tol1 <= O And O < F_tol2 Then F_v = F_v2
         If F_tol2 < O Then F_v = F_v3
         F_v = 128 + F_v
      Else
         F_v = 128
      End If
   Else
      F_v = 128
   End If
End Function


Sub Ao(byval M As Byte , Byval N As Byte)
   A = M
   I = N * 16
   Shift A , Left , 12
   Shift M , Right , 4
   M = M + I
   A = A + M

   Dac_sync = 0
   Spiout A , 2
   Dac_sync = 1
   Dac_ldac = 0
   Dac_ldac = 1

End Sub



Gant_learn:
   Disable Ovf0
   Disable Ovf2
   Disable Ovf1
   Reset Watchdog

   Print "lnv"
   Gant_v = 0
   Gant_v = Get_diag()
   Gant_v = Gant_v - 128

   Call Ao(gant_v , 0)

   Portd = 127
   Gant_fr = 1
   Gant_rr = 0

   Aa = Gant_lower_limit - 1
   Bb = Gant_lower_limit + 1
   Do
      Gosub Gant_pot_read
      X = Inkey()
      If X = 119 Then
         Do
         Loop
      End If
      If Gant_f1 >= Aa And Gant_f1 <= Bb Then
         Gant_zpnt = Gant_co
         Gant_upper_limit = Gant_f2
         Exit Do
      'Elseif Gant_co>
      End If
   Loop

   Aa = Gant_upper_limit - 1
   Bb = Gant_upper_limit + 1
   Do
      Gosub Gant_pot_read
      X = Inkey()
      If X = 119 Then
         Do
         Loop
      End If
      If Gant_f1 >= Aa And Gant_f1 <= Bb Then
         Gant_length = Gant_co - Gant_zpnt
         Gant_length = Abs(gant_length)
         Exit Do
      Elseif Gant_f1 > 3800 Then
         Print "lnk"
         Do
         Loop
      End If
   Loop


   Gant_v = 0
   Call Ao(gant_v , 0)
   Gant_fr = 0
   Gant_rr = 0

   Gant_fine_length = Gant_upper_limit - Gant_lower_limit
'(
Gant_zpnt = 3550
Gant_length = 86
Gant_fine_length = 2191
 ')

   Do
      Print "c43" ; Gant_zpnt
      Waitus Send_delay
      Print "c44" ; Gant_length
      Waitus Send_delay
      Print "c45" ; Gant_fine_length
      Waitus Send_delay

      Sok = 1

      S1 = ""
         Do
            X = waitkey()
            If X = 47 Then Exit Do
            S1 = S1 + Chr(x)
         Loop
      A = Val(s1)
      If A <> Gant_zpnt Then Sok = 0

      S1 = ""
         Do
            X = waitkey()
            If X = 47 Then Exit Do
            S1 = S1 + Chr(x)
         Loop
      Z = Val(s1)
      If Z <> Gant_length Then Sok = 0

      S1 = ""
         Do
            X = waitkey()
            If X = 47 Then Exit Do
            S1 = S1 + Chr(x)
         Loop
      A = Val(s1)
      If A <> Gant_fine_length Then Sok = 0

      If Sok = 1 Then Exit Do
   Loop

   Print "lok"
   Enable Ovf0
   Enable Ovf2
   Reset Watchdog
Return

Gant_pot_read:

   Reset Watchdog
   For I = 1 To 3
      Adc_rd(i) = 0
      Adc_rd(i) = Adc_read(i , 8)
   Next I

   Gant_co = Adc_rd(1)
   Gant_f1 = Adc_rd(2)
   Gant_f2 = Adc_rd(3)
Return


Collim_learn:
   Disable Ovf0
   Disable Ovf2
   Disable Ovf1

   Reset Watchdog

   Print "lnv"
   Collim_v = 0
   Collim_v = Get_diag()

   Call Ao(collim_v , 1)

   Portd = 127
   Gant_fr = 0
   Gant_rr = 0
   Collim_on = 0

   Aa = Collim_lower_limit - 1
   Bb = Collim_lower_limit + 1
   Do
      Gosub Collim_pot_read
      X = Inkey()
      If X = 119 Then
         Do
         Loop
      End If
      If Collim_f1 >= Aa And Collim_f1 <= Bb Then
         Collim_zpnt = Collim_co
         Collim_upper_limit = Collim_f2
         Exit Do
      End If
   Loop


   Aa = Collim_upper_limit - 1
   Bb = Collim_upper_limit + 1
   Do
      Gosub Collim_pot_read
      X = Inkey()
      If X = 119 Then
         Do
         Loop
      End If
      If Collim_f1 >= Aa And Collim_f1 <= Bb Then
         Collim_length = Collim_co - Collim_zpnt
         Collim_length = Abs(collim_length)
         Exit Do
      Elseif Collim_f1 > 3800 Then
         Print "lnk"
         Do
         Loop
      End If
   Loop


   Collim_v = 128
   Call Ao(collim_v , 1)
   Collim_on = 1

   Collim_fine_length = Collim_upper_limit - Collim_lower_limit
'(
Collim_zpnt = 324
Collim_length = 205
Collim_fine_length = 2197
')

   Do
      Print "c46" ; Collim_zpnt
      Waitus Send_delay
      Print "c47" ; Collim_length
      Waitus Send_delay
      Print "c48" ; Collim_fine_length
      Waitus Send_delay

      Sok = 1

      S1 = ""
         Do
            X = waitkey()
            If X = 47 Then Exit Do
            S1 = S1 + Chr(x)
         Loop
      A = Val(s1)
      If A <> Collim_zpnt Then Sok = 0

      S1 = ""
         Do
            X = waitkey()
            If X = 47 Then Exit Do
            S1 = S1 + Chr(x)
         Loop
      Z = Val(s1)
      If Z <> Collim_length Then Sok = 0

      S1 = ""
         Do
            X = waitkey()
            If X = 47 Then Exit Do
            S1 = S1 + Chr(x)
         Loop
      A = Val(s1)
      If A <> Collim_fine_length Then Sok = 0


      If Sok = 1 Then Exit Do
   Loop

   Print "lok"
   Enable Ovf0
   Enable Ovf2
   Enable Ovf1
   Reset Watchdog

Return

Collim_pot_read:

   Reset Watchdog
   For I = 4 To 6
      Adc_rd(i) = 0
      Adc_rd(i) = Adc_read(i , 8)
   Next I

   Collim_co = Adc_rd(4)
   Collim_f1 = Adc_rd(5)
   Collim_f2 = Adc_rd(6)
Return


T0_postscale:
   Incr T0_p
   If T0_p = 255 Then
      T0_p = T0_def
      Gosub Positioning
   End If
Return

T1_postscale:
   Incr T1_p
   If T1_p = 255 Then
      T1_p = T1_def
      Gosub Read_positions
   End If
Return


T2_postscale:
   Incr T2_p
   If T2_p = 255 Then
      T2_p = T2_def
      Gosub Srout
   End If
Return


Function X1_check()as Bit:
   Local A As Long
   X1_check = 1

   If X1_co < X1_in_end_c And X1_v > 128 Then X1_check = 0
   If X1_co > X1_out_end_c And X1_v < 128 Then X1_check = 0


   If X1_tt = 0 Then
      A = X1_co - X2_co
      A = Abs(a)
      If A < X_jaws_c And X1_v > 128 Then
         X1_check = 0
         X1_tt = 4
      End If
   Elseif X1_v > 128 Then
      X1_check = 0
   End If
End Function


Function x2_check()as Bit:
   Local A As Long
   x2_check = 1

   If x2_co < x2_in_end_c And x2_v > 128 Then x2_check = 0
   If x2_co > x2_out_end_c And x2_v < 128 Then x2_check = 0


   If x2_tt = 0 Then
      A = X2_co - X1_co
      A = Abs(a)
      If A < X_jaws_c And x2_v > 128 Then
         x2_check = 0
         x2_tt = 4
      End If
   Elseif x2_v > 128 Then
      X2_check = 0
   End If
End Function

Function y1_check()as Bit:
   Local A As Long
   y1_check = 1

   If y1_co < y1_in_end_c And y1_v > 128 Then y1_check = 0
   If y1_co > y1_out_end_c And y1_v < 128 Then y1_check = 0


   If y1_tt = 0 Then
      A = y1_co - y2_co
      A = Abs(a)
      If A < y_jaws_c And y1_v > 128 Then
         y1_check = 0
         y1_tt = 4
      End If
   Elseif y1_v > 128 Then
      y1_check = 0
   End If
End Function


Function y2_check()as Bit:
   Local A As Long
   y2_check = 1

   If y2_co < y2_in_end_c And y2_v > 128 Then y2_check = 0
   If y2_co > y2_out_end_c And y2_v < 128 Then y2_check = 0


   If y2_tt = 0 Then
      A = y2_co - y1_co
      A = Abs(a)
      If A < y_jaws_c And y2_v > 128 Then
         y2_check = 0
         y2_tt = 4
      End If
   Elseif y2_v > 128 Then
      Y2_check = 0
   End If
End Function