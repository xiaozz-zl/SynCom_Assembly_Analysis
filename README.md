# 🧬 合成芽孢杆菌菌群社会互作研究复现项目

[![R-v4.x](https://img.shields.io/badge/R-v4.x+-blue.svg)](https://www.r-project.org/)
[![iMeta](https://img.shields.io/badge/Journal-iMeta(2025)-green.svg)](https://onlinelibrary.wiley.com/journal/28339010)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

## 👥 小组信息
*   **小组名称**：合成菌群研究小组
*   **小组成员**：
    *   韦荣耀 (2025303120153)
    *   赵甜甜 (2025303120144)
    *   马  微 (2025303120060)
    *   张一鸣 (2025303120095)
    *   沐禹廷（2025303120121）

---

## 📖 项目背景
本项目基于 2025 年发表于 **iMeta** 杂志的科研成果进行复现研究。
     关于本项目复现结果的解读均保存在all qmd文件夹中
*   **论文标题**：*Bacterial social interactions in synthetic Bacillus consortia enhance plant growth*（合成芽孢杆菌菌群中的细菌社会互作促进植物生长）
*   **核心科学问题**：探索不同亲缘关系的芽孢杆菌如何通过社会互作（Social Interactions）组装成合成菌群（SynComs），以及这些互作如何协同促进黄瓜生长。
*   **复现基础**：
    *   **数据公开**：测序数据已托管至 NCBI (**PRJNA879238**, **PRJNA1248407**)。
    *   **代码透明**：原始分析脚本已在 [GitHub](https://github.com/yanliu2023/iMeta/) 开源。
    *   **逻辑严密**：涵盖从数据标准化到高级统计绘图的全流程。
   
  ---

## 🛠 环境配置
本项目采用 `renv` 框架进行依赖管理，以确保在不同设备上分析结果的可重复性（Reproducibility）。

### 快速开始
1.  克隆本项目并打开 R 项目文件 (`.Rproj`)。
2.  在 R 控制台（Console）中执行以下命令以同步环境：
    ```R
    # 安装并恢复项目所需的 R 包依赖
    renv::restore()
    ```

---

## 📂 项目结构
```text
IMETA (Project Root)
├── all qmd/           # 核心分析文档 (.qmd)
├── Figure 2/          # 图 2 相关脚本与中间数据
├── Figure 4/          # 图 4 相关脚本与中间数据
├── Figure 5/          # 图 5 相关脚本与中间数据
├── local_packages/    # 本地专用 R 包
├── output/            # 可视化结果输出
│   ├── biolog_PCA.pdf # 碳源利用 PCA 矢量图
│   └── biolog_PCA.png # 碳源利用 PCA 预览图
├── renv/              # renv 环境库
├── .Rprofile          # R 启动配置文件
├── README.md          # 项目主说明文档
└── renv.lock          # 依赖版本锁定文件
