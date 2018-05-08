import './styles/cards.scss'
import './styles/style.scss'
import { Main } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

Main.embed(document.getElementById('root'));

registerServiceWorker();
