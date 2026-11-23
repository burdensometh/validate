# 供方数据入仓门禁（隔离 / 合格清单）

把 `validate::confront()` 的行级结果转成可执行的隔离与合格清单：
合格记录先入仓，问题记录带原行键与修复理由隔离；修复后按原键重验并更新隔离状态。

## 门禁口径（rules.R）

1. `key_present`：原行键 `id` 必须存在（隔离与重验靠它定位记录）；
2. `fin_w1..fin_w4`：财务连续块 `turnover, other.rev, total.rev, staff.costs,
   total.costs, profit` 内，任一连续 3 个字段不得全部为 NA。

规则表达式与修复理由由 `FIN_BLOCK` / `WIN` 同源生成，改口径只改这两处。
规则无法评估（结果为 NA）的记录按不通过处理。

## 运行

```bash
# 1) 首次拆分：合格清单 + 隔离清单 + 状态台账
Rscript split.R [供方CSV] [输出目录]
#    默认供方CSV = ../pkg/data/SBS2000.csv，输出目录 = ./out

# 2) 修复后按原键重验，更新隔离状态
Rscript revalidate.R <修复CSV> [供方CSV] [输出目录]
#    示例：Rscript revalidate.R examples/repaired_records.csv
```

## 产物（输出目录）

| 文件 | 说明 |
| --- | --- |
| `qualified.csv` | 合格记录（全部规则通过），第一批入仓 |
| `quarantine.csv` | 隔离记录：原行键 + 失败规则 + 修复理由 + 记录当前值；重验后刷新为仍 open 的记录 |
| `quarantine_reasons.csv` | 隔离理由长表（id x 失败规则），便于派单 |
| `quarantine_status.csv` | 隔离状态台账：`open`（待修）/ `resolved`（重验通过） |
| `qualified_from_repair.csv` | 重验通过、解除隔离的记录，第二批入仓 |
| `quarantine_events.csv` | 事件日志：`quarantined` / `resolved` / `still_open` |

## 修复与重验约定

- 修复文件与供方数据同一 schema（列名一致，分号分隔，`NA` 表示缺失），
  `id` 为原行键；可只提交部分隔离记录。
- 重验只针对台账中的隔离键：全部规则通过 → `resolved` 并进入
  `qualified_from_repair.csv`；仍有失败 → 保持 `open`，理由刷新为最新结果；
  未提交修复的键状态不变；不在台账中的键会被警告并跳过。
- 供方/修复数据的原行键若缺失或重复，流水线直接中止（键是隔离定位的基础）。
