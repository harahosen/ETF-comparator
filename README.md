# ETF composition comparator

Personal project born to learn about Haskell and a bit about finance. It takes two CSV holding lists, normalizes them, and reports cosine, weighted Jaccard and overlap metrics.

Built using two specific providers (iShares and StateStreet) as reference, but it is possible to use custom files with a different structure.

**N.b.:** this is a small POC and the ETF comparator is not a complete tool, please do not take investing decisions based on it

## Build the Haskell project

```bash
stack build
```

## Run

```bash
stack exec etf-comparator -- input/YYYYMMDD-IS-<name>.csv input/YYYYMMDD-SS-<name>.xlsx
```

or, using a customizable config file:

```bash
stack exec etf-comparator -- --config config.yaml <file1> <file2>
```

## Preparing input files

The comparator accepts both raw and pre-adapted files. Raw iShares/State Street files can be in `.csv` or `.xlsx` format and are reconciled automatically before comparison. Custom files must be `.csv`.

- **Input filenames**: `YYYYMMDD-PROVIDER-<name>.csv` or `.xlsx` where `PROVIDER` is:
  - `IS` — iShares
  - `SS` — State Street
  - `CF` — custom file

- **Required header columns**:
  - Asset id: `isin`, `ticker`, `symbol`, `name` (for `IS`/`SS`) or `assetId` (for `CF`)
  - Weight: `weight`, `weight %`, `weight (%)`, `weight(%)`, `market weight`

- **Weight values**: plain numbers with `.` as decimal separator. Percent signs, commas as thousand/decimal separators, or extra whitespace will fail. Weights do not need to sum to `1.0`; resolved holdings are renormalized to `1.0` when their total is not within the configured `tolerance`.

- `input/asset-mapping.csv` is optional. When absent, raw asset ids are treated as canonical.

See `input/` for sample files.

## How it works

1. **Ingestion**: `etf-comparator` loads both holding lists (CSV or XLSX) and turns them into in-memory tables.
2. **Reconcile**: it auto-detects the header row in each table by looking for asset, name and weight columns, then matches the secondary fund's names to the primary fund's tickers, producing two `ticker,weight` tables.
3. **Resolve**: each reconciled asset id is mapped to a canonical one; when no mapping exists, the reconciled id itself is used.
4. **Merge & normalize**: duplicate canonical ids are summed and weights are renormalized to `1.0` if their total deviates by more than the configured `tolerance`.
5. **Compare**: the two normalized funds are compared with cosine similarity, weighted Jaccard similarity and overlap ratio.

## Configuration

`etf-comparator` can be configured through a YAML file (see `--config` above). Available fields:

- `assetMappingFile`: path to an optional `raw,canonical` CSV. When missing, raw ids are treated as canonical.
- `outputDirectory`: directory where result files are written (`output` by default).
- `tolerance`: maximum allowed deviation of total weight from `1.0` before renormalization is triggered.

## Output

On success, a `output/comparison-<timestamp>.csv` file is created:

```csv
timestamp,etf_file_1,etf_file_2,cosine_similarity,weighted_jaccard_similarity,overlap_ratio
```

On failure, an `output/error-<timestamp>.csv` file is created:

```csv
timestamp,error_file,error_list
```

Error rows contain a JSON-escaped list of issues such as invalid filenames, missing required columns, non-numeric weights, or normalization problems.

## Docker

A basic `Dockerfile` is provided. Build the image and run the tool inside a container:

```bash
docker build -t etf-comparator .
```

Run the comparator on raw or adapted iShares/State Street files:

```bash
docker run --rm -v "$PWD:/data" etf-comparator etf-comparator \
  /data/input/YYYYMMDD-IS-<name>.csv /data/input/YYYYMMDD-SS-<name>.xlsx
```

The command writes its output into the mounted `/data` directory.
