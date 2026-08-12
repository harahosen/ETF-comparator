# ETF composition comparator

Personal project born to learn about Haskell and a bit about finance. It takes two CSV holding lists, normalizes them, and reports cosine, weighted Jaccard and overlap metrics.

Built using two specific providers (iShares and StateStreet) as reference, but it is possible to use custom files with a different structure.

**N.b.:** this is a small POC and the ETF comparator is not a complete tool, please do not take investing decisions based on it

## Build the Haskell project

```bash
stack build
```

## Run

**File reconciliation (optional, could be done manually)**

```bash
stack exec reconcile -- Input/YYYYMMDD-IS-<name>.csv Input/YYYYMMDD-SS-<name>.xlsx
```

**Comparison, using or not a customizable config file** 

```bash
stack exec etf-comparator -- Input/YYYYMMDD-IS-<name>-adapted.csv Input/YYYYMMDD-SS-<name>-adapted.csv
```
or

```bash
stack exec etf-comparator -- --config config.yaml <file1> <file2>
```


## Preparing CSV files for the comparison

`reconcile` must be run first to adapt raw `IS` and `SS` files into `etf-comparator` input.

- **Input filenames**: `YYYYMMDD-PROVIDER-<name>.csv` or `.xlsx` where `PROVIDER` is:
  - `IS` — iShares
  - `SS` — State Street
  - `CF` — custom file

- **Required header columns**:
  - Asset id: `isin`, `ticker`, `symbol`, `name` (for `IS`/`SS`) or `assetId` (for `CF`)
  - Weight: `weight`, `weight %`, `weight (%)`, `weight(%)`, `market weight`

- **Weight values**: plain numbers with `.` as decimal separator. Percent signs, commas as thousand/decimal separators, or extra whitespace will fail. Weights do not need to sum to `1.0`; resolved holdings are renormalized to `1.0` when their total is not within the configured `tolerance`.

- `Input/asset-mapping.csv` is optional. When absent, raw asset ids are treated as canonical.

See `Input/` for sample files.

## How it works

1. **Reconcile**: `reconcile` reads two raw holding lists (CSV or XLSX), auto-detects the header row by looking for asset, name and weight columns, then matches the secondary fund's names to the primary fund's tickers. It writes `ticker,weight` CSV files ready for the comparator.
2. **Resolve**: `etf-comparator` loads each adapted file, resolves every raw asset id to a canonical one and falls back to the raw id itself when no mapping exists.
3. **Merge & normalize**: duplicate canonical ids are summed and weights are renormalized to `1.0` if their total deviates by more than the configured `tolerance`.
4. **Compare**: the two normalized funds are compared with cosine similarity, weighted Jaccard similarity and overlap ratio.

## Configuration

`etf-comparator` can be configured through a YAML file (see `--config` above). Available fields:

- `assetMappingFile`: path to an optional `raw,canonical` CSV. When missing, raw ids are treated as canonical.
- `outputDirectory`: directory where result files are written (`Output` by default).
- `tolerance`: maximum allowed deviation of total weight from `1.0` before renormalization is triggered.

## Output

On success, a `Output/comparison-<timestamp>.csv` file is created:

```csv
timestamp,etf_file_1,etf_file_2,cosine_similarity,weighted_jaccard_similarity,overlap_ratio
```

On failure, an `Output/error-<timestamp>.csv` file is created:

```csv
timestamp,error_file,error_list
```

Error rows contain a JSON-escaped list of issues such as invalid filenames, missing required columns, non-numeric weights, or normalization problems.

## Docker

A basic `Dockerfile` is provided. Build the image and run the tools inside a container:

```bash
docker build -t etf-comparator .
```

Run `reconcile` on raw iShares/State Street files:

```bash
docker run --rm -v "$PWD/Input:/data/Input" etf-comparator reconcile \
  /data/Input/YYYYMMDD-IS-<name>.csv /data/Input/YYYYMMDD-SS-<name>.xlsx /data/Input
```

Run the comparator on adapted files:

```bash
docker run --rm -v "$PWD:/data" etf-comparator etf-comparator \
  /data/Input/YYYYMMDD-IS-<name>-adapted.csv /data/Input/YYYYMMDD-SS-<name>-adapted.csv
```

Both commands write their output into the mounted `/data` directory.
