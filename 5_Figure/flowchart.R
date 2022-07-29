library(DiagrammeR)
grViz(diagram = "digraph flowchart {
  graph [splines = ortho]
  node [fontname = Helvetica, shape = rectangle, ]
  tab1 [label = '@@1']
  tab2 [label = '@@2']
  tab3 [label = '@@3']
  tab4 [label = '@@4']
  tab5 [label = '@@5']
  tab6 [label = '@@6']
  tab7 [label = '@@7']
  tab1 -> tab2 -> tab3 -> {tab4 tab5 tab6};
  {tab4 tab5} -> tab7
  tab6 ->tab2
}
  
  [1]: '设置停止收集数据的贝叶斯因子阈值'
  [2]: '收集数据'    
  [3]: '根据先验计算贝叶斯因子'  
  [4]: '大于阈值' 
  [5]: '达到可收集的最大样本量' 
  [6]: '小于阈值'  
  [7]: '报告相应信息' 
  ")
