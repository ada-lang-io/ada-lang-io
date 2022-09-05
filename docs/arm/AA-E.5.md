---
sidebar_position:  173
---

# E.5  Partition Communication Subsystem

{AI95-00273-01} [The Partition Communication Subsystem (PCS) provides facilities for supporting communication between the active partitions of a distributed program. The package System.RPC is a language-defined interface to the PCS.] 

Reason: The prefix RPC is used rather than RSC because the term remote procedure call and its acronym are more familiar. 


#### Static Semantics

The following language-defined library package exists: 

```ada
{AI12-0241-1} {AI12-0302-1} with Ada.Streams; -- see 13.13.1
package System.RPC
   with Nonblocking =&gt False, Global =&gt in out synchronized is

```

```ada
   type Partition_Id is range 0 .. implementation-defined;

```

```ada
   Communication_Error : exception;

```

```ada
   type Params_Stream_Type (
      Initial_Size : Ada.Streams.Stream_Element_Count) is new
      Ada.Streams.Root_Stream_Type with private;

```

```ada
   procedure Read(
      Stream : in out Params_Stream_Type;
      Item : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset);

```

```ada
   procedure Write(
      Stream : in out Params_Stream_Type;
      Item : in Ada.Streams.Stream_Element_Array);

```

```ada
   -- Synchronous call
   procedure Do_RPC(
      Partition  : in Partition_Id;
      Params     : access Params_Stream_Type;
      Result     : access Params_Stream_Type);

```

```ada
   -- Asynchronous call
   procedure Do_APC(
      Partition  : in Partition_Id;
      Params     : access Params_Stream_Type);

```

```ada
   -- The handler for incoming RPCs
   type RPC_Receiver is access procedure(
      Params     : access Params_Stream_Type;
      Result     : access Params_Stream_Type);

```

```ada
   procedure Establish_RPC_Receiver(
      Partition : in Partition_Id;
      Receiver  : in RPC_Receiver);

```

```ada
private
   ... -- not specified by the language
end System.RPC;

```

A value of the type Partition_Id is used to identify a partition. 

Implementation defined: The range of type System.RPC.Partition_Id.

An object of the type Params_Stream_Type is used for identifying the particular remote subprogram that is being called, as well as marshalling and unmarshalling the parameters or result of a remote subprogram call, as part of sending them between partitions.

[The Read and Write procedures override the corresponding abstract operations for the type Params_Stream_Type.]


#### Dynamic Semantics

The Do_RPC and Do_APC procedures send a message to the active partition identified by the Partition parameter. 

Implementation Note: It is assumed that the RPC interface is above the message-passing layer of the network protocol stack and is implemented in terms of it. 

After sending the message, Do_RPC blocks the calling task until a reply message comes back from the called partition or some error is detected by the underlying communication system in which case Communication_Error is raised at the point of the call to Do_RPC. 

Reason: Only one exception is defined in System.RPC, although many sources of errors might exist. This is so because it is not always possible to distinguish among these errors. In particular, it is often impossible to tell the difference between a failing communication link and a failing processing node. Additional information might be associated with a particular Exception_Occurrence for a Communication_Error. 

Do_APC operates in the same way as Do_RPC except that it is allowed to return immediately after sending the message.

Upon normal return, the stream designated by the Result parameter of Do_RPC contains the reply message.

The procedure System.RPC.Establish_RPC_Receiver is called once, immediately after elaborating the library units of an active partition (that is, right after the elaboration of the partition) if the partition includes an RCI library unit, but prior to invoking the main subprogram, if any. The Partition parameter is the Partition_Id of the active partition being elaborated. The Receiver parameter designates an implementation-provided procedure called the RPC-receiver which will handle all RPCs received by the partition from the PCS. Establish_RPC_Receiver saves a reference to the RPC-receiver; when a message is received at the called partition, the RPC-receiver is called with the Params stream containing the message. When the RPC-receiver returns, the contents of the stream designated by Result is placed in a message and sent back to the calling partition. 

Implementation Note: It is defined by the PCS implementation whether one or more threads of control should be available to process incoming messages and to wait for their completion. 

Implementation Note: At link-time, the linker provides the RPC-receiver and the necessary tables to support it. A call on Establish_RPC_Receiver is inserted just before the call on the main subprogram. 

Reason: The interface between the PCS (the System.RPC package) and the RPC-receiver is defined to be dynamic in order to allow the elaboration sequence to notify the PCS that all packages have been elaborated and that it is safe to call the receiving stubs. It is not guaranteed that the PCS units will be the last to be elaborated, so some other indication that elaboration is complete is needed. 

If a call on Do_RPC is aborted, a cancellation message is sent to the called partition, to request that the execution of the remotely called subprogram be aborted. 

To be honest: The full effects of this message are dependent on the implementation of the PCS. 

This paragraph was deleted.{AI12-0241-1} 


#### Implementation Requirements

The implementation of the RPC-receiver shall be reentrant[, thereby allowing concurrent calls on it from the PCS to service concurrent remote subprogram calls into the partition]. 

Reason: There seems no reason to allow the implementation of RPC-receiver to be nonreentrant, even though we don't require that every implementation of the PCS actually perform concurrent calls on the RPC-receiver. 

{8652/0087} {AI95-00082-01} An implementation shall not restrict the replacement of the body of System.RPC. An implementation shall not restrict children of System.RPC. [The related implementation permissions in the introduction to Annex A do not apply.] 

Reason: The point of System.RPC is to let the user tailor the communications mechanism without requiring changes to or other cooperation from the compiler. However, implementations can restrict the replacement of language-defined units. This requirement overrides that permission for System.RPC. 

{8652/0087} {AI95-00082-01} If the implementation of System.RPC is provided by the user, an implementation shall support remote subprogram calls as specified. 

Discussion: {AI95-00273-01} If the implementation takes advantage of the implementation permission to use a different specification for System.RPC, it still needs to use it for remote subprogram calls, and allow the user to replace the body of System.RPC. It just isn't guaranteed to be portable to do so in Ada 2005 - an advantage which was more theoretical than real anyway. 


#### Documentation Requirements

The implementation of the PCS shall document whether the RPC-receiver is invoked from concurrent tasks. If there is an upper limit on the number of such tasks, this limit shall be documented as well, together with the mechanisms to configure it (if this is supported). 

This paragraph was deleted.

Documentation Requirement: Whether the RPC-receiver is invoked from concurrent tasks, and if so, the number of such tasks.


#### Implementation Permissions

The PCS is allowed to contain implementation-defined interfaces for explicit message passing, broadcasting, etc. Similarly, it is allowed to provide additional interfaces to query the state of some remote partition (given its partition ID) or of the PCS itself, to set timeouts and retry parameters, to get more detailed error status, etc. These additional interfaces should be provided in child packages of System.RPC. 

Implementation defined: Implementation-defined interfaces in the PCS.

{AI12-0444-1} A body for the package System.RPC is not required to be supplied by the implementation. 

Reason: It is presumed that a body for the package System.RPC might be extremely environment specific. Therefore, we do not require that a body be provided by the (compiler) implementation. The user will have to write a body, or acquire one, appropriate for the target environment. 

{AI95-00273-01} {AI05-0299-1} An alternative declaration is allowed for package System.RPC as long as it provides a set of operations that is substantially equivalent to the specification defined in this subclause. 

Reason: Experience has proved that the definition of System.RPC given here is inadequate for interfacing to existing distribution mechanisms (such as CORBA), especially on heterogeneous systems. Rather than mandate a change in the mechanism (which would break existing systems), require implementations to support multiple mechanisms (which is impractical), or prevent the use of Annex E facilities with existing systems (which would be silly), we simply make this facility optional.

One of the purposes behind System.RPC was that knowledgeable users, rather than compiler vendors, could create this package tailored to their networks. Experience has shown that users get their RPC from vendors anyway; users have not taken advantage of the flexibility provided by this defined interface. Moreover, one could compare this defined interface to requiring Ada compilers to use a defined interface to implement tasking. No one thinks that the latter is a good idea, why should anyone believe that the former is?

{AI05-0299-1} Therefore, this subclause is made optional. We considered deleting the subclause outright, but we still require that users may replace the package (whatever its interface). Also, it still provides a useful guide to the implementation of this feature. 


#### Implementation Advice

Whenever possible, the PCS on the called partition should allow for multiple tasks to call the RPC-receiver with different messages and should allow them to block until the corresponding subprogram body returns. 

Implementation Advice: The PCS should allow for multiple tasks to call the RPC-receiver.

The Write operation on a stream of type Params_Stream_Type should raise Storage_Error if it runs out of space trying to write the Item into the stream. 

Implementation Advice: The System.RPC.Write operation should raise Storage_Error if it runs out of space when writing an item.

Implementation Note: An implementation could also dynamically allocate more space as needed, only propagating Storage_Error if the [allocator](./AA-4.8#S0164) it calls raises Storage_Error. This storage could be managed through a controlled component of the stream object, to ensure that it is reclaimed when the stream object is finalized. 

NOTE 1   The package System.RPC is not designed for direct calls by user programs. It is instead designed for use in the implementation of remote subprograms calls, being called by the calling stubs generated for a remote call interface library unit to initiate a remote call, and in turn calling back to an RPC-receiver that dispatches to the receiving stubs generated for the body of a remote call interface, to handle a remote call received from elsewhere. 


#### Incompatibilities With Ada 95

{AI95-00273-01} The specification of System.RPC can now be tailored for an implementation. If a program replaces the body of System.RPC with a user-defined body, it might not compile in a given implementation of Ada 2005 (if the specification of System.RPC has been changed). 


#### Wording Changes from Ada 95

{8652/0087} {AI95-00082-01} Corrigendum: Clarified that the user can replace System.RPC. 

