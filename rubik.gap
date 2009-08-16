cube  :=  Group(
( 1, 3, 8, 6)( 2, 5, 7, 4)( 9,33,25,17)(10,34,26,18)(11,35,27,19),
( 9,11,16,14)(10,13,15,12)( 1,17,41,40)( 4,20,44,37)( 6,22,46,35),
(17,19,24,22)(18,21,23,20)( 6,25,43,16)( 7,28,42,13)( 8,30,41,11),
(25,27,32,30)(26,29,31,28)( 3,38,43,19)( 5,36,45,21)( 8,33,48,24),
(33,35,40,38)(34,37,39,36)( 3, 9,46,32)( 2,12,47,29)( 1,14,48,27),
(41,43,48,46)(42,45,47,44)(14,22,30,38)(15,23,31,39)(16,24,32,40) ) ; 
Size( cube ) ; 
Collected( Factors( Size( cube ) ) ) ; 
SizeScreen( [71, ] ) ;   
orbits  :=  Orbits( cube, [1..48] ) ; 
cube1  :=  Action( cube, orbits[1] ) ; 
NrMovedPoints( cube1 ) ; 
Size( cube1 ) ; 
corners  :=  Blocks( cube1, MovedPoints( cube1 ) ) ; 
blockhom1  :=  ActionHomomorphism( cube1, corners, OnSets ) ; 
cube1b  :=  Image( blockhom1 ) ; 
Size( cube1b ) ; 
Factors( Size( Kernel( blockhom1 ) ) ) ; 
IsElementaryAbelian( Kernel( blockhom1 ) ) ; 
cmpl1  :=  Complementclasses( cube1, Kernel( blockhom1 ) ) ; 
cmpl1  :=  cmpl1[1] ;   
Size( cmpl1 ) ; 
Size( Intersection( cmpl1, Kernel( blockhom1 ) ) ) ; 
ClosureGroup( cmpl1, Kernel( blockhom1 ) ) = cube1 ; 
IsBijective( RestrictedMapping( blockhom1, cmpl1 ) ) ; 
(1,7,22) in cube1 ; 
(1,7,22)(2,20,14) in cube1 ; 
cube2  :=  Action( cube, orbits[2] ) ;   
Size( cube2 ) ; 
edges  :=  Blocks( cube2, MovedPoints( cube2 ) ) ; 
blockhom2  :=  ActionHomomorphism( cube2, edges, OnSets ) ;   
cube2b  :=  Image( blockhom2 ) ;   
Size( cube2b ) ; 
Factors( Size( Kernel( blockhom2 ) ) ) ; 
IsElementaryAbelian( Kernel( blockhom2 ) ) ; 
cmpl2  :=  Complementclasses( cube2, Kernel( blockhom2 ) ) ;   
Length( cmpl2 ) ; 
(1,11) in cube2 ; 
(1,11)(2,17) in cube2 ; 
Size( cube ) ; 
Size( cube1 ) * Size( cube2 ) ; 
(17,19)(11,8)(6,25) in cube ; 
(7,28)(18,21) in cube ; 
(17,19)(11,8)(6,25)(7,28)(18,21) in cube ; 
z  :=  Centre( cube ) ; 
f  :=  FreeGroup("t","l","f","r","e","b") ; 
hom  :=  GroupHomomorphismByImages( f, cube, GeneratorsOfGroup(f),
GeneratorsOfGroup(cube) ) ; 
pre  :=  PreImagesRepresentative( hom, z.1 ) ; 
Length( pre ) ; 
pre  :=  PreImagesRepresentative( hom, (17,19)(11,8)(6,25)(7,28)(18,21) ) ; 
Length( pre ) ; 
r  :=  Random( cube ) ; 
pre  :=  PreImagesRepresentative( hom, r ) ; 
Length( pre ) ; 
im := Image( hom, pre ) ; 
im = r ; 
