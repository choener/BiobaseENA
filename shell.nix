(import ./.).shellFor {
  packages = p: [ p.BiobaseENA ];
  withHoogle = true;
}

