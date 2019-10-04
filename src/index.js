import 'tachyons'
import './index.css'
// import { Elm } from './Main.elm'
import { Elm } from './Explorer.elm'

const app = Elm.Explorer.init({
  flags: {
    cache: localStorage.getItem('elm-todoist-clone-v2-cache'),
    now: Date.now(),
  },
  node: document.getElementById('root'),
})

app.ports.setCache.subscribe(cacheString => {
  localStorage.setItem('elm-todoist-clone-v2-cache', cacheString)
})

document.addEventListener('click', e => {
  const onclick = e.target.dataset['onclick']
  if (onclick) {
    console.log('click', onclick)
    eval(onclick)
  }
})
