; ModuleID = 'Module'


 


define external ccc  void @test_repeat()    {
entry:
  %i = alloca i16, align 4 
  br label %k.2 
k.1:
  br label %k.3 
k.2:
  %l.1 = load  i16, i16* %i, align 4 
  %l.2 = add   i16 %l.1, 1 
  store  i16 %l.2, i16* %i, align 4 
  %l.4 = load  i16, i16* %i, align 4 
  %l.5 = icmp eq i16 %l.4, 4 
  br i1 %l.5, label %k.1, label %k.2 
k.3:
  ret void 
}


define external ccc  void @__refresh_global()    {
entry:
  ret void 
}