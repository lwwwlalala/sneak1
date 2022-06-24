library(beepr)
dev.new()
source(file="game.r") #加载游戏框架
# 初始化环境变量
init<-function(){
  e<<-new.env()
  e$stage<-0 #场景
  e$width<-e$height<-20  #切分格子
  e$step<-1/e$width #步长
  e$m<-matrix(rep(0,e$width*e$height),nrow=e$width)  #点矩阵
  e$dir<-e$lastd<-'up' # 移动方向
  e$head<-c(2,2) #初始蛇头
  e$lastx<-e$lasty<-2 # 初始化蛇头上一个点
  e$tail<-data.frame(x=c(),y=c())#初始蛇尾
  
  e$col_furit<-2 #水果颜色
  e$col_head<-4 #蛇头颜色
  e$col_tail<-8 #蛇尾颜色
  e$col_path<-0 #路颜色
  e$score<0 #分数
  e$col_obstruction<-9#黑色障碍物
}


# 获得矩阵的索引值
index<-function(col) which(e$m==col)

# 游戏中
stage1<-function(){
  e$stage<-1
  #得分
  score<-function(){   #设置score函数
    #在游戏中页面建立text函数添加当前的分的文字，位置在页面右上角，字体大小为0.9，颜色为灰色
    text(0.8,0.9,label=paste("[当前得分：",nrow(e$tail),"分]"),cex=0.9,col=8)
    
  }  
  # 随机的水果点
  furit<-function(){
    if(length(index(e$col_furit))<=0){ #不存在水果
      idx<-sample(index(e$col_path),1)
      
      fx<-ifelse(idx%%e$width==0,10,idx%%e$width)
      fy<-ceiling(idx/e$height)
      e$m[fx,fy]<-e$col_furit
      
      print(paste("furit idx",idx))
      print(paste("furit axis:",fx,fy))
    }
  }
 #障碍物的移动
  obstruction<-function(){
    if(length(index(e$col_obstruction))<=0){
      idx<-sample(index(e$col_path),1)
      # if(obsteuctionMove)
      # {
      #   e$m[,"x"]<<-e$m[,"x"]+1
      #   colnames(e$m<-matrix)<<-as.numeric(colnames(e$m<-matrix))+1
      # }
      fx<-10
      fy<-10
      e$m[fx,fy]<-e$col_obstruction
      
      print(paste("idx",idx+1))
      print(paste("obstruction axis:",fx,fy))
    }
  }
  
  # 检查失败
  fail<-function(){
    # head出边界
    if(length(which(e$head<1))>0 | length(which(e$head>e$width))>0){
      print("game over: Out of ledge.")
      keydown('q')
      beep(1)
      return(TRUE)
    }
    
    # head碰到tail
    if(e$m[e$head[1],e$head[2]]==e$col_tail){
      print("game over: head hit tail")
      keydown('q')
      beep(1)
      return(TRUE)
    }
    
    #head碰到障碍物
    if(e$m[e$head[1],e$head[2]]==e$col_obstruction){#当head碰到障碍物
      print("game over: head hit obstruction")  #输出失败信息
      keydown('q')
      beep(1)
      return(TRUE)
    }
    #tail碰到障碍物
    # if(e$m[e$tail[1],e$tail[2]]==e$col_obstruction){
    #   print("game over: tail hit obstruction")
    #   keydown('q')
    #   return(TRUE)
    # }
    beep(1)
    return(FALSE)
  }
  
  
  # 贪吃蛇的头部
  head<-function(){
    e$lastx<-e$head[1]
    e$lasty<-e$head[2]
    
    # 方向操作
    if(e$dir=='up') e$head[2]<-e$head[2]+1
    if(e$dir=='down') e$head[2]<-e$head[2]-1
    if(e$dir=='left') e$head[1]<-e$head[1]-1
    if(e$dir=='right') e$head[1]<-e$head[1]+1
    
  }
  
  # 贪吃蛇的身体
  body<-function(){
    e$m[e$lastx,e$lasty]<-0
    e$m[e$head[1],e$head[2]]<-e$col_head #snake
    if(length(index(e$col_furit))<=0){ #不存在水果
      e$tail<-rbind(e$tail,data.frame(x=e$lastx,y=e$lasty))
    }
    # if(length(nrow(e$tail)/3==0)){ #3个水果
    #   e$tail<-rbind(e$tail,data.frame(x=e$lastx,y=e$lasty))+2
    # }
    if(nrow(e$tail)>0) { #如果有尾巴
      e$tail<-rbind(e$tail,data.frame(x=e$lastx,y=e$lasty))
      e$m[e$tail[1,]$x,e$tail[1,]$y]<-e$col_path
      e$tail<-e$tail[-1,]
      e$m[e$lastx,e$lasty]<-e$col_tail
    }
    
    
    print(paste("snake idx",index(e$col_head)))
    print(paste("snake axis:",e$head[1],e$head[2]))
  }


  # 画布背景
  drawTable<-function(){
    plot(0,0,xlim=c(0,1),ylim=c(0,1),type='n',xaxs="i", yaxs="i")
  }
  
  # 根据矩阵画数据
  drawMatrix<-function(){
    idx<-which(e$m>0)
    px<- (ifelse(idx%%e$width==0,e$width,idx%%e$width)-1)/e$width+e$step/2
    py<- (ceiling(idx/e$height)-1)/e$height+e$step/2
    pxy<-data.frame(x=px,y=py,col=e$m[idx])
    points(pxy$x,pxy$y,col=pxy$col,pch=15,cex=4.4)
  }
  
  furit()
  head()

  if(!fail()){
    body()
    drawTable()
    drawMatrix()  
    score()#增加score
    obstruction()
  }
}


# 开机画图
stage0<-function(){
  e$stage<-0
  plot(0,0,xlim=c(0,1),ylim=c(0,1),type='n',xaxs="i", yaxs="i")
  text(0.5,0.7,label="Snake Game",cex=5)
  text(0.5,0.4,label="Any keyboard to start",cex=2,col=4)
  text(0.5,0.3,label="Up,Down,Left,Rigth to control direction",cex=2,col=2)
  text(0.2,0.05,label="Author:DanZhang",cex=1)
  text(0.5,0.05,label="http://blog.fens.me",cex=1)
}

# 结束画图
stage2<-function(){
  e$stage<-2
  plot(0,0,xlim=c(0,1),ylim=c(0,1),type='n',xaxs="i", yaxs="i")
  text(0.5,0.7,label="Game Over",cex=5)
  text(0.5,0.4,label="Space to restart, q to quit.",cex=2,col=4)
  text(0.5,0.3,label=paste("Congratulations! You have eat",nrow(e$tail),"fruits!"),cex=2,col=2)
  text(0.2,0.05,label="Author:DanZhang",cex=1)
  text(0.5,0.05,label="http://blog.fens.me",cex=1)
  beep(1)
}
#暂停画图
stage3<-function(){  #建立stage3函数
  e$stage<-3         #调用e$stage
  plot(0,0,xlim=c(0,1),ylim=c(0,1),type='n',xaxs="i", yaxs="i")  #plot画图
  text(0.5,0.7,label="pause",cex=5)                              #text写：pause
  text(0.5,0.4,label="Press the p key again to end the pause.",cex=2,col=4)#text写提醒
}

# 键盘事件
keydown<-function(K){
  print(paste("keydown:",K,",stage:",e$stage));
  
  if(e$stage==0){ #开机画面
    init()
    stage1()
    return(NULL)
  }  
  
  if(e$stage==2){ #结束画面
    if(K=="q") q()
    else if(K==' ') stage0()  
    return(NULL)
  } 
  
  if(e$stage==1){ #游戏中
    if(K == "q") {
      stage2()
    } else  {
      if(tolower(K) %in% c("up","down","left","right")){
        e$lastd<-e$dir
        e$dir<-tolower(K)
        stage1()  
      }
    } 
  }
  if(e$stage==1){ #在游戏中页面时
    if(K == "p"){ #按p键
      stage3()    #跳转到stage3暂停页面
      return(NULL)
    }
  }
  if(e$stage==3){ #在暂停页面时
    if(K == "p"){ #按p键
      stage1()    #跳转到stage1游戏中页面
    }
  }
  return(NULL)
}

#######################################
# RUN  
#######################################  

run<-function(){
  par(mai=rep(0,4),oma=rep(0,4))
  e<<-new.env()
  stage0()
  
  # 注册事件
  getGraphicsEvent(prompt="Snake Game",onKeybd=keydown)
}

run()