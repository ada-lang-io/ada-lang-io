with RP.GPIO;
with Pico;

procedure Main is
begin
   RP.GPIO.Enable;

   Pico.LED.Configure (RP.GPIO.Output);
   Pico.LED.Set;

   loop
      Pico.LED.Toggle;
      delay 0.1;
   end loop;
end Main;
