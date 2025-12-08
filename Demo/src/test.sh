#!/bin/bash

echo -e "\n\e[1;36m--- 1. Basic Latin & ASCII ---\e[0m"
echo -e "Hello World 123 !@#\$%^&*()"

echo -e "\n\e[1;36m--- 2. Cyrillic ---\e[0m"
echo -e "Привет, мир! Ўзбекистон"

echo -e "\n\e[1;36m--- 3. Box Drawing ---\e[0m"
echo -e "┌───┬───┐"
echo -e "│ A │ B │"
echo -e "├───┼───┤"
echo -e "│ C │ D │"
echo -e "└───┴───┘"

echo -e "\n\e[1;36m--- 4. Simple Emojis ---\e[0m"
echo -e "Rocket: 🚀  Fire: 🔥  Smile: 😎  Heart: ❤️"

echo -e "\n\e[1;36m--- 5. ZWJ Sequences ---\e[0m"
# Реальные ZWJ последовательности
echo -e "Family: 👨‍👩‍👧‍👦"
echo -e "Technologist: 👨‍💻"
echo -e "Flag rainbow: 🏳️‍🌈"

echo -e "\n\e[1;36m--- 6. Wide Characters (CJK) ---\e[0m"
echo -e "日本語 中文 한국어"

echo -e "\n\e[1;36m--- 7. Color Test ---\e[0m"
for i in {0..15}; do
  printf "\e[48;5;${i}m %3d \e[0m" $i
done
echo ""

echo "日本語 中文 한국어"
echo "Wide: ＡＢＣ"  # Fullwidth Latin
echo "Mix: Hello世界Test"

echo -e "\n\e[1;32m✓ Test complete!\e[0m"