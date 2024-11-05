source('Misc/example_creation.R')
source('scripts/distances_calculator.R')
source('scripts/fragmentation.R')
source('scripts/tests/fragments_comparison_test.R')
source('https://raw.githubusercontent.com/hbrpaulo/Misc/refs/heads/main/format_sig.R')
database <- fragmentation(distances_calculator(example1))

# Print trend results

