# Effects in Kitten

Kitten's effect system lets you safely and statically manage
mutation, I/O, memory operations, etc.

## Syntax

Syntactically, effects appear after the return types of a
function type.  Each effect is prefixed with `+`.

Effects variables behave as row variables: they may be
scoped and are polymorphic.

## Standard library

### I/O: files, sockets, etc.

    module IO

    // Type var 'w' refers to 'world'.
    effect w IO

    type w Handle
    type OpenMode

    // Creating Handles.
    def openFile (FilePath OpenMode -> w Handle +w IO -Exception)
    def withFile (.r FilePath OpenMode (forall w: .r w Handle -> .s +w IO +e) -> .s +x IO +e -Exception)

    // Manipulating handles.
    def close (w Handle -> +w IO -Exception)
    def readBytes (Size w Handle -> Uint8[] +w IO -Exception)
    def writeBytes (Uint8[] w Handle -> +w IO -Exception)
    def readToPtr (Size h Ptr w Handle -> +h Write +w IO -Exception)
    def writeFromPtr (Size h Ptr w Handle -> +h Read +w IO -Exception)
    def tell (w Handle -> Offset +w IO -Exception)
    def seek (Offset w Handle -> +w IO -Exception)

### Mutable state

    module State

    // Type var 'z' refers to the last thing we want, state.  =]
    effect z New
    effect z Read
    effect z Write

    effect z State = +h New +h Read +h Write

    commute z New y New
    commute z Read y Read

    type a z Mut

    def new (a -> a z Mut +z New)
    def read (a z Mut -> a +z Read)
    def write (a (a z Mut) -> +z Write)

    def runState (.r (forall z: .r -> .s +z State +e) -> .s +e)

### Explicit, mutable C memory

    // All of these functions are inheritly unsafe.
    module UnsafeMemory

    // Type var 'h' refers to 'heap'.
    effect h Alloc
    effect h Free
    effect h Read
    effect h Write

    effect h Memory = +h Alloc +h Free +h Read +h write

    commute h Alloc i Alloc
    commute h Read i Read
    commute h Free i Free

    type h Ptr

    // C <stdlib.h> routines.
    def malloc (Size -> h Ptr +h Alloc)
    def realloc (Size h Ptr -> h Ptr +h Memory)
    def reallocf (Size h Ptr -> h Ptr +h Memory)
    def free (h Ptr -> +h Free)

    // C <string.h> routines.
    def memcpy(Size h Ptr i Ptr -> +h Read +i Write)
    def memcmp(Size h Ptr i Ptr -> Ordering +h Read +i Read)

    // Manipulating memory.
    def read (h Ptr -> Uint8 +h Read)
    def write (Uint8 h Ptr -> +h Write)
    def readAt (Offset h Ptr -> Uint8 +h Read)
    def writeAt (Offset Uint8 h Ptr -> +h Write)

    // Manipulating pointers.
    def addPtr (Offset h Ptr -> h Ptr)
    def diffPtr (h Ptr i Ptr -> Offset)

    // Managing effects.
    def runMemory (.r (forall h: .r -> .s +h Memory +e) -> .s +e)
    def unsafeWithMemory (forall h: .r (.r -> .s +h Memory +e) -> .s +e)

    // Misc.
    def withMalloc (.r Size (.r h Ptr -> .s +h Memory +e) -> .s +h Memory +e)
