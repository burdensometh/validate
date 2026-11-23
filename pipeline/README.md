# 证书数据质量流水线

针对上游证书编号尾部偶发填空格的问题。值班员已确认"去除尾部空白"这一类
安全修正可以批处理；本流水线按**登记表驱动**的方式执行，未登记的规则绝不
修改数据。

## 运行

```sh
Rscript pipeline/run_pipeline.R \
    [--data=CSV] [--rules=YML] [--registry=YML] [--outdir=DIR]
```

默认读取本目录下的 `data/certificates_upstream.csv`、`rules.yml`、
`corrections.yml`，结果写入 `out/`。依赖 R 包：`validate`、`yaml`。

## 行为准则

1. **只转换命中已登记规则的行**：某行必须 (a) 未通过某条规则，且 (b) 该
   规则在 `corrections.yml` 中登记了修正方式，才会被转换。登记的 `fix`
   还必须在 `run_pipeline.R` 的 `FIXES` 白名单中有实现，否则跳过。
2. **只重跑受影响的校验**：修正后仅重跑引用了被修改列的规则（由
   `validate::variables()` 推导），前后结果都记录。
3. **仍失败的行保留待处理**：修正后仍失败（或结果无法判定 NA）的行写入
   `out/pending_rows.csv`，附失败规则清单，等待人工处理。
4. **审批外规则只报告**：未登记的规则只出现在报告与日志中，数据不被修改。

## 文件

| 文件 | 说明 |
| --- | --- |
| `rules.yml` | 全部校验规则（validate 包 YAML 格式） |
| `corrections.yml` | 已批准的安全修正登记表（当前仅 `cert_no_format` 去尾部空白） |
| `run_pipeline.R` | 流水线执行器 |
| `data/certificates_upstream.csv` | 上游批次样例 |
| `out/certificates_corrected.csv` | 修正后的完整批次 |
| `out/pending_rows.csv` | 仍失败、待人工处理的行 |
| `out/validation_report.csv` | 各规则前后通过/失败/无法判定对照 |
| `out/pipeline.log` | 执行日志（含逐行改写记录） |

## 新增一类可批处理修正

1. 经值班员确认后，在 `corrections.yml` 的 `corrections:` 下登记：
   规则名、目标列、`fix` 名称、批准人与日期；
2. 在 `run_pipeline.R` 的 `FIXES` 白名单中实现同名修正函数；
3. 重新运行流水线，检查 `out/validation_report.csv` 的前后对照。
