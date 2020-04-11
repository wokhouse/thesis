const fs = require('fs');

const { operations: genderThemesRaw } = require('./Pronoun and Gender Survey - Q2_ What is your gender_ - Topics');
const { operations: pronounsThemesRaw } = require('./Pronoun and Gender Survey - Q3_ What are your pronouns_ - Topics');
const { operations: sexualThemesRaw } = require('./Pronoun and Gender Survey - Q19_ What is your sexual orientation_ - Topics');

const genderThemes = genderThemesRaw.map(t => t.topic.label);
const pronounsThemes = pronounsThemesRaw.map(t => t.topic.label);
const sexualThemes = sexualThemesRaw.map(t => t.topic.label);

const themes = {
  gender: genderThemes,
  pronouns: pronounsThemes,
  sexual: sexualThemes,
};

fs.writeFile("themes.json", JSON.stringify(themes), (err) => {
  if (err) console.error(err);
  console.log('themes parsed and saved to themes.json');
});
