; ModuleID = 'Module'


 


define external ccc  void @test_fb({i32, i32}*  %_self)    {
entry:
  %l.0 = getelementptr inbounds {i32, i32}, {i32, i32}* %_self, i32 0, i32 0 
  %l.1 = load  i32, i32* %l.0, align 4 
  %l.2 = sext i16 0 to i32  
  %l.3 = icmp sgt i32 %l.1, %l.2 
  br i1 %l.3, label %k.2, label %k.3 
k.1:
  br label %k.27 
k.10:
  store  i1 0, i1* %l.22, align 4 
  br label %k.9 
k.11:
  store  i1 1, i1* %l.22, align 4 
  br label %k.9 
k.12:
  br i1 %l.36, label %k.11, label %k.10 
k.13:
  %l.25 = getelementptr inbounds {i32, i32}, {i32, i32}* %_self, i32 0, i32 1 
  %l.26 = load  i32, i32* %l.25, align 4 
  %l.27 = sext i16 333333 to i32  
  %l.28 = icmp sgt i32 %l.26, %l.27 
  %l.29 = alloca i1, align 1 
  br i1 %l.28, label %k.18, label %k.15 
k.14:
  %l.36 = load  i1, i1* %l.29, align 4 
  br label %k.12 
k.15:
  store  i1 0, i1* %l.29, align 4 
  br label %k.14 
k.16:
  store  i1 1, i1* %l.29, align 4 
  br label %k.14 
k.17:
  br i1 %l.35, label %k.16, label %k.15 
k.18:
  %l.32 = getelementptr inbounds {i32, i32}, {i32, i32}* %_self, i32 0, i32 1 
  %l.33 = load  i32, i32* %l.32, align 4 
  %l.34 = sext i16 444444 to i32  
  %l.35 = icmp slt i32 %l.33, %l.34 
  br label %k.17 
k.19:
  %l.45 = load  i1, i1* %l.38, align 4 
  br i1 %l.45, label %k.25, label %k.26 
k.2:
  %l.4 = getelementptr inbounds {i32, i32}, {i32, i32}* %_self, i32 0, i32 1 
  %l.5 = getelementptr inbounds {i32, i32}, {i32, i32}* %_self, i32 0, i32 1 
  %l.6 = load  i32, i32* %l.5, align 4 
  %l.7 = sext i16 7 to i32  
  %l.8 = add   i32 %l.6, %l.7 
  store  i32 %l.8, i32* %l.4, align 4 
  br label %k.1 
k.20:
  store  i1 0, i1* %l.38, align 4 
  br label %k.19 
k.21:
  store  i1 1, i1* %l.38, align 4 
  br label %k.19 
k.22:
  br i1 %l.44, label %k.21, label %k.20 
k.23:
  %l.41 = getelementptr inbounds {i32, i32}, {i32, i32}* %_self, i32 0, i32 1 
  %l.42 = load  i32, i32* %l.41, align 4 
  %l.43 = sext i16 555555 to i32  
  %l.44 = icmp ne i32 %l.42, %l.43 
  br label %k.22 
k.24:
  br label %k.1 
k.25:
  %l.46 = getelementptr inbounds {i32, i32}, {i32, i32}* %_self, i32 0, i32 1 
  %l.47 = getelementptr inbounds {i32, i32}, {i32, i32}* %_self, i32 0, i32 1 
  %l.48 = load  i32, i32* %l.47, align 4 
  %l.49 = sext i16 8 to i32  
  %l.50 = add   i32 %l.48, %l.49 
  store  i32 %l.50, i32* %l.46, align 4 
  br label %k.24 
k.26:
  %l.52 = getelementptr inbounds {i32, i32}, {i32, i32}* %_self, i32 0, i32 1 
  %l.53 = getelementptr inbounds {i32, i32}, {i32, i32}* %_self, i32 0, i32 1 
  %l.54 = load  i32, i32* %l.53, align 4 
  %l.55 = sext i16 9 to i32  
  %l.56 = add   i32 %l.54, %l.55 
  store  i32 %l.56, i32* %l.52, align 4 
  br label %k.24 
k.3:
  %l.10 = getelementptr inbounds {i32, i32}, {i32, i32}* %_self, i32 0, i32 0 
  %l.11 = load  i32, i32* %l.10, align 4 
  %l.12 = sext i16 111111 to i32  
  %l.13 = icmp sgt i32 %l.11, %l.12 
  %l.14 = alloca i1, align 1 
  br i1 %l.13, label %k.8, label %k.5 
k.4:
  %l.21 = load  i1, i1* %l.14, align 4 
  %l.22 = alloca i1, align 1 
  br i1 %l.21, label %k.13, label %k.10 
k.5:
  store  i1 0, i1* %l.14, align 4 
  br label %k.4 
k.6:
  store  i1 1, i1* %l.14, align 4 
  br label %k.4 
k.7:
  br i1 %l.20, label %k.6, label %k.5 
k.8:
  %l.17 = getelementptr inbounds {i32, i32}, {i32, i32}* %_self, i32 0, i32 1 
  %l.18 = load  i32, i32* %l.17, align 4 
  %l.19 = sext i16 222222 to i32  
  %l.20 = icmp slt i32 %l.18, %l.19 
  br label %k.7 
k.9:
  %l.37 = load  i1, i1* %l.22, align 4 
  %l.38 = alloca i1, align 1 
  br i1 %l.37, label %k.23, label %k.20 
k.27:
  ret void 
}


define external ccc  void @__refresh_global()    {
entry:
  ret void 
}