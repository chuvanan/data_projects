

require(luckynumbers)
require(data.table)

aaaaaaa <- lucky_number_generator(7, x1 == x2 & x2 == x3 & x3 == x4 & x4 == x5 & x5 == x6 & x6 == x7)
aa1a2a3a4a5a6 <- lucky_number_generator(7, x1 + 1 == x2 & x1 + 2 == x3 & x1 + 3 == x4 & x1 + 4 == x5 & x1 + 5 == x6 & x1 + 6 == x7)
aaaaaa1a2 <- lucky_number_generator(7, x1 == x2 & x2 == x3 & x3 == x4 & x4 == x5 & x1 + 1 == x6 & x1 + 2 == x7)
aaaaa1a2a3 <- lucky_number_generator(7, x1 == x2 & x2 == x3 & x3 == x4 & x1 + 1 == x5 & x1 + 2 == x6 & x1 + 3 == x7)
aa1a2aa1a2a3 <- lucky_number_generator(7, x1 + 1 == x2 & x1 + 2 == x3 & x1 == x4 & x1 + 1 == x5 & x1 + 2 == x6 & x1 + 3 == x7)
aaaaaab <- lucky_number_generator(7, x1 == x2 & x2 == x3 & x3 == x4 & x4 == x5 & x5 == x6 & x6 != x7)
aaaaabb <- lucky_number_generator(7, x1 == x2 & x2 == x3 & x3 == x4 & x4 == x5 & x5 != x6 & x6 == x7)
aaaabbb <- lucky_number_generator(7, x1 == x2 & x2 == x3 & x3 == x4 & x4 != x5 & x5 == x6 & x6 == x7)
aaabbbb <- lucky_number_generator(7, x1 == x2 & x2 == x3 & x3 != x4 & x4 == x5 & x5 == x6 & x6 == x7)
aabbbbb <- lucky_number_generator(7, x1 == x2 & x2 != x3 & x3 == x4 & x4 == x5 & x5 == x6 & x6 == x7)
aaabcbc <- lucky_number_generator(7, x1 == x2 & x2 == x3 & x4 == x6 & x5 == x7 & x1 != x4 & x1 != x5 & x4 != x5)
ababccc <- lucky_number_generator(7, x1 == x3 & x2 == x4 & x5 == x6 & x6 == x7 & x1 != x2 & x1 != x5 & x2 != x5)
aaabbcc <- lucky_number_generator(7, x1 == x2 & x2 == x3 & x4 == x5 & x6 == x7 & x1 != x4 & x1 != x6 & x4 != x6)
aabbccc <- lucky_number_generator(7, x1 == x2 & x3 == x4 & x5 == x6 & x6 == x7 & x1 != x3 & x1 != x5 & x3 != x5)
aabbbaa <- lucky_number_generator(7, x1 == x2 & x3 == x4 & x4 == x5 & x1 == x6 & x6 == x7 & x1 != x3)
aababaa <- lucky_number_generator(7, x1 == x2 & x3 == x5 & x1 == x4 & x1 == x6 & x6 == x7 & x1 != x3)
abababa <- lucky_number_generator(7, x1 == x3 & x1 == x5 & x1 == x7 & x2 == x4 & x2 == x6 & x1 != x2)
aababaa <- lucky_number_generator(7, x1 == x2 & x1 == x4 & x1 == x6 & x1 == x7 & x3 == x5 & x1 != x3)
aaabaaa <- lucky_number_generator(7, x1 == x2 & x1 == x3 & x1 == x5 & x1 == x6 & x1 == x7 & x1 != x4)

nrow(aaaaaaa) + nrow(aa1a2a3a4a5a6) + nrow(aaaaaa1a2) + nrow(aaaaa1a2a3) +
    nrow(aaaaaab) + nrow(aaaaabb) + nrow(aaaabbb) + nrow(aaabbbb) +
    nrow(aabbbbb) + nrow(aaabcbc) + nrow(ababccc) + nrow(aaabbcc) +
    nrow(aabbccc) + nrow(aabbbaa) + nrow(aababaa) + nrow(abababa) +
    nrow(aababaa) + nrow(aaabaaa)

all_v7 <- rbindlist(list(aaaaaaa, aa1a2a3a4a5a6, aaaaaa1a2, aaaaa1a2a3, aaaaaab,
                         aaaaabb, aaaabbb, aaabbbb, aabbbbb, aaabcbc, ababccc,
                         aaabbcc, aabbccc, aabbbaa, aababaa, abababa, aababaa,
                         aaabaaa))

head(all_v7)
