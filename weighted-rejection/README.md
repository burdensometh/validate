# 按批加权拒收率（weighted rejection rate per batch）

抽样检测的货物按批发量配权：简单平均行失败率把每条记录等权对待，会低估
大批次（大批发量）的异常。本目录从**真实校验结果**（`validate::confront`
的输出）与**抽样权重**计算每个批次的加权拒收率。

## 数据与口径

- **数据**：`validate` 包内置的 `retailers`（60 家抽样企业，真实随包数据）。
- **批（batch）**：`size` 企业规模类别（sc0–sc3），作为批次键；
  聚合函数本身接受任意批次键。
- **校验结果**：`validate::confront()` 对 7 条真实规则
  （源自本仓库 `jsspaper/rules.R`）的逐记录评估。每条记录的拒收状态：
  任一规则为 `FALSE` → 拒收；全部规则无法评估 → `NA`（状态未知）；否则通过。
- **抽样权重**：`turnover / incl.prob`，即批发量（turnover）乘以设计权重
  （1/包含概率）——该抽样记录所代表的总体批发量。turnover 缺失的记录
  权重不可用，计入 `n_weight_missing`。

## 输出列（每批一行）

| 列 | 含义 |
|---|---|
| `batch` | 批次键 |
| `n_records` | 批次内源记录总数 |
| `n_included` | **纳入记录**：进入拒收率计算的记录数（权重有限为正 且 拒收状态已知） |
| `effective_weight` | **有效权重**：纳入记录的权重之和（加权拒收率的分母） |
| `missing_weight` | **缺失权重**：权重有限为正、但因校验结果缺失而未能进入计算的权重质量 |
| `n_weight_missing` | 权重缺失/不可用（NA、NaN、Inf 或 ≤ 0）的记录数 |
| `weighted_reject_rate` | **加权拒收率** = Σ(w·fail) / Σ(w)（对纳入记录） |
| `simple_reject_rate` | 对照列：等权（简单平均）行失败率 |

## 两条保证

1. **没有有限正权重的小组保持可识别**：这样的批次不丢弃，以
   `n_included = 0`、`effective_weight = 0`、`weighted_reject_rate = NA`
   的行出现在结果中（未观测到的 factor 水平同样保留）。
2. **重排源行不改变统计**：批次键排序后输出，组内先按 `(weight, fail)`
   排序再做浮点累加，结果与输入行序无关（`identical()` 级别一致，
   由测试在玩具数据与真实数据上验证）。

## 运行

```bash
# 从仓库根目录执行
Rscript weighted-rejection/weighted_rejection.R [输出csv路径]
Rscript weighted-rejection/test_weighted_rejection.R
```

默认输出：`weighted-rejection/output/weighted_rejection_by_batch.csv`。

## 文件

- `weighted_rejection.R` — 聚合函数 + 真实数据管线（可 source 也可直接运行）
- `test_weighted_rejection.R` — 断言式测试（手工核算、行序不变性、独立重算交叉验证）
- `output/weighted_rejection_by_batch.csv` — 生成的按批结果
