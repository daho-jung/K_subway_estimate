Rscript version => R-4.0.2
-----------------------


프로그램 실행 순서
1. python -> main.py

  >통계청 api를 통해 json 데이터를 가공
  >
  > pop_by_year.csv 생성, C:/pp 디렉토리 생성
  > 
  > subprocess. -> call Rscript
  
2. Rscript -> rscript.R
  
  >(read csv files and processing)
  >
  >위에서 생성한 pop_by_year.csv 와 지하철 통계를 이용해 만든 파일 profit.csv를 read
  >
  >데이터에 대한 분석을 통해 유의성 검정
  >
  >(draw graph and save png files by ggsave)
  >
  >데이터로 그래프 그림, png파일로 저장
    
----------------------

ggsave("~.pdf")  #<- does not work. save to png files   pdf에서는 범례, 축 한글 표시가 안됨.

python subprocess로 Rscript가 실행되지 않는듯 함. 

(python subprocess -> call rscript, save png file -> FAIL)
      테스트 해봤더니 csv read,write OK
