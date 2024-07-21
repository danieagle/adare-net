(optional)
pandoc --wrap=auto --dpi=300 -V geometry:a4paper,hmargin=5.2mm,vmargin=13mm \
  --from=markdown --to=pdf adare_net.md -o adare_net.pdf