---
sidebar_position:  82
---

# 9.11  Example of Tasking and Synchronization


#### Examples

{AI12-0440-1} The following example defines a buffer protected object to smooth variations between the speed of output of a producing task and the speed of input of some consuming task. For instance, the producing task can have the following structure:

```ada
task Producer;

```

```ada
{AI95-00433-01} task body Producer is
   Person : Person_Name; -- see 3.10.1
begin
   loop
      ... --  simulate arrival of the next customer
      Buffer.Append_Wait(Person);
      exit when Person = null;
   end loop;
end Producer;

```

{AI12-0440-1} and the consuming task can have the following structure:

```ada
task Consumer;

```

```ada
{AI95-00433-01} task body Consumer is
   Person : Person_Name;
begin
   loop
      Buffer.Remove_First_Wait(Person);
      exit when Person = null;
      ... --  simulate serving a customer
   end loop;
end Consumer;

```

{AI95-00433-01} The buffer object contains an internal array of person names managed in a round-robin fashion. The array has two indices, an In_Index denoting the index for the next input person name and an Out_Index denoting the index for the next output person name.

{AI95-00433-01} The Buffer is defined as an extension of the Synchronized_Queue interface (see 3.9.4), and as such promises to implement the abstraction defined by that interface. By doing so, the Buffer can be passed to the Transfer class-wide operation defined for objects of a type covered by Queue'Class.

```ada
{AI12-0178-1} type Person_Name_Array is array (Positive range &lt&gt)
   of Person_Name;  -- see 3.10.1

```

```ada
{AI95-00433-01} protected Buffer is new Synchronized_Queue with  -- see 3.9.4
   entry Append_Wait(Person : in Person_Name);
   entry Remove_First_Wait(Person : out Person_Name);
   function Cur_Count return Natural;
   function Max_Count return Natural;
   procedure Append(Person : in Person_Name);
   procedure Remove_First(Person : out Person_Name);
private
   Pool      : Person_Name_Array(1 .. 100);
   Count     : Natural := 0;
   In_Index, Out_Index : Positive := 1;
end Buffer;

```

```ada
{AI95-00433-01} protected body Buffer is
   entry Append_Wait(Person : in Person_Name)
      when Count &lt Pool'Length is
   begin
      Append(Person);
   end Append_Wait;

```

```ada
{AI95-00433-01}    procedure Append(Person : in Person_Name) is
   begin
      if Count = Pool'Length then
         raise Queue_Error with "Buffer Full";  -- see 11.3
      end if;
      Pool(In_Index) := Person;
      In_Index       := (In_Index mod Pool'Length) + 1;
      Count          := Count + 1;
   end Append;

```

```ada
{AI95-00433-01}    entry Remove_First_Wait(Person : out Person_Name)
      when Count &gt 0 is
   begin
      Remove_First(Person);
   end Remove_First_Wait;

```

```ada
{AI95-00433-01}    procedure Remove_First(Person : out Person_Name) is
   begin
      if Count = 0 then
         raise Queue_Error with "Buffer Empty"; -- see 11.3
      end if;
      Person    := Pool(Out_Index);
      Out_Index := (Out_Index mod Pool'Length) + 1;
      Count     := Count - 1;
   end Remove_First;

```

```ada
{AI95-00433-01}    function Cur_Count return Natural is
   begin
       return Buffer.Count;
   end Cur_Count;

```

```ada
{AI95-00433-01}    function Max_Count return Natural is
   begin
       return Pool'Length;
   end Max_Count;
end Buffer;

```

