import 'tachyons'
import './index.css'
import { Elm } from './Main.elm'
// import { Elm } from './Explorer.elm'
import { pathOr } from 'ramda'

const app = Elm.Main.init({
  flags: {
    cache: localStorage.getItem('elm-todoist-clone-v2-cache'),
    now: Date.now(),
  },
  node: document.getElementById('root'),
})

import fire from './fire'

const pubOnAuthStateChanged = pub('onAuthStateChanged', app)
const pubOnFireTodoList = pub('onFireTodoList', app)

function sleep(ms, andThen) {
  return setTimeout(andThen, ms)
}

fire.onAuthStateChanged(data =>
  sleep(2000, () => pubOnAuthStateChanged(data)),
)

fire.onUserCollection('todos', s => {
  console.debug('snap', s)
  pubOnFireTodoList(s.docs.map(ds => ds.data()))
})

sub(
  'setCache',
  cacheString => {
    localStorage.setItem('elm-todoist-clone-v2-cache', cacheString)
  },
  app,
)

sub('signIn', fire.signIn, app)
sub('signOut', fire.signOut, app)
sub('logError', err => console.error('Elm:', err), app)
sub('firePushTodoList', todoList => fire.setAll('todos', todoList), app)

function sub(name, fn, app) {
  const subscribe = pathOr(null, ['ports', name, 'subscribe'])(app)
  if (!subscribe) {
    console.warn('Port not found : ', name)
    return
  }
  subscribe(fn)
}

function pub(name, app) {
  const send = pathOr(null, ['ports', name, 'send'])(app)
  if (!send) {
    console.warn('Port not found : ', name)
    return function(data) {
      console.warn('Send Error: Port not found : ', name, data)
    }
  }
  return send
}
