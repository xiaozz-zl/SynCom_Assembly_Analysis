## 小组基本信息

* 小组名称：合成菌群研究小组
* 小组成员：韦荣耀 2025303120153
   项目名称：基于《Bacterial social interactions in synthetic Bacillus consortia enhance plant growth》项目复现
  
## 项目内容

该论文发表在 2025 年的iMeta杂志上，其原始数据和分析代码均在 GitHub 上发布（(https://github.com/yanliu2023/iMeta/。)），具备可重复研究的基础，接下来准备以其材料为基础，尝试复现相关结果中的一部分。
论文标题：Bacterial social interactions in synthetic Bacillus consortia enhance plant growth（合成芽孢杆菌菌群中的细菌社会互作促进植物生长）
发表期刊：iMeta (2025)
核心内容：研究不同亲缘关系的芽孢杆菌如何组装成合成菌群，并验证其对黄瓜生长的促进作用。
复现价值：
数据全：16S rRNA 和 gyrA 基因测序数据已上传 NCBI。
代码全：所有原始数据和分析脚本已托管于 GitHub。
逻辑清：从数据标准化到统计绘图，流程非常标准。
原始数据 (NCBI)：
BioProject 登录号：PRJNA879238 (扩增子测序数据) 和 PRJNA1248407 (分离菌株序列)。
仓库地址：https://github.com/yanliu2023/iMeta/。
## 环境配置
本项目使用 renv 进行依赖管理以确保可复现性。
克隆本项目后，请运行以下命令安装所需依赖：
renv::restore()
## 结果解读
