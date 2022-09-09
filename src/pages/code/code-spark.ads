with Interfaces;

package Xoshiro128 with Pure, SPARK_Mode => On is
   use type Interfaces.Unsigned_64;

   type Generator is limited private;

   procedure Next (S : in out Generator; Value : out Interfaces.Unsigned_32)
     with Global  => null,
          Depends => (S => S, Value => S);

   procedure Reset (S : out Generator; Seed : Interfaces.Unsigned_64)
     with Global  => null,
          Depends => (S => Seed),
          Pre     => Seed /= 0;

private
   type Generator is array (0 .. 3) of Interfaces.Unsigned_32;
end Xoshiro128;
