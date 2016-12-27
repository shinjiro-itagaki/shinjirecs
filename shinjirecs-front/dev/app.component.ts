/// <reference path="../src/lib/jquery.d.ts" />
/// <reference path="./../config/config.d.ts" />
import {Component,OnInit} from '@angular/core'
import {provideRouter,ROUTER_DIRECTIVES, Router, Event, RoutesRecognized, NavigationStart, NavigationEnd, RouterStateSnapshot, ActivatedRouteSnapshot} from '@angular/router'
import {CORE_DIRECTIVES,NgClass} from '@angular/common'
import {AllService} from './all.service'
import {FlashServiceObserver} from './flash.service'
import {NetworkObserver} from './network_observer'

let cssFadeOut = "0% {margin-left: 0; opacity: 0} 10% {opacity: 1} 30% {opacity: 1} 100% {margin-left: 0; opacity: 0}";
let thinkingLiveHref="http://idealive.bbt757.com/"; // 思考ライブのリンク先をここに登録する

@Component({
  selector: 'my-app',
  styles: [
    ``
  ],

  template: `
    <div>
    <div id="update-view-trigger" (click)="updateView()" style="display:none;" class="click-btn"></div>
    <header> </header>
    <div id="main">
    <router-outlet></router-outlet>
    </div>
    <footer>
    <nav>
    <ul>
    <li><a [routerLink]="['/']">番組一覧</a></li>
    </ul>      
    </nav>
    <p><small class="copyright">Copyright &copy; 2016 Shinjiro Itagaki. All Rights reserved. </small></p>
    </footer>
    `,
  directives: [CORE_DIRECTIVES,ROUTER_DIRECTIVES, NgClass]
})

export class AppComponent implements OnInit, FlashServiceObserver {

  public screen_size_width : number = null; 

  _flash_hidden: boolean = true;
  _flash_message: string = null;
  _flash_error:   string = null;
  _flash_sw: boolean = true;
  _flash_timer: any = null;
  _hide_header = true; // ここをfalseにすると、ヘッダを表示しないページでも一瞬だけ表示されてしまうので、初期状態ではヘッダを隠すようにする。
  _hide_login_btn_on_header = true;

  private _network_observer : NetworkObserver = null;
  
  constructor(private _router: Router, private _service: AllService) {
    this._network_observer = new NetworkObserver(_service.api(), 2000);
    this._network_observer.onErrorOccured = () => {
      alert("接続エラーが発生しました。");
    }

    this._network_observer.onReconnected = () => {
      this.displayNetworkError(false);
      alert("接続が回復しました。");
      this._service.initCurrentComponentDataAsync();
    }
    
    _service.api().onNetworkError = () => {
      this.displayNetworkError(true);
      _service.nowLoading(false);
      this._network_observer.onError();
    }

    _service.api().onTimeoutError = () => {
      _service.nowLoading(false);
    }

    _router.events.subscribe(
      (e: Event) : void => {
	this._hide_header = false; // 非表示フラグを初期化
	this._hide_login_btn_on_header = true;
	if(e instanceof RoutesRecognized){
	  this._service.viewport_user_scalable = true;
	  this._service.nowLoading(true);
	}else{
	  if(e instanceof NavigationEnd){
            let path : string = window.location.pathname;	    
	    this._service.updateViewportUserScalable();
	    let rsss : RouterStateSnapshot = this._router.routerState.snapshot;
	    let arss : ActivatedRouteSnapshot = rsss.firstChild(rsss.root);
	    this._hide_header = !!arss.data['hide_header'];
	    this._hide_login_btn_on_header = !!arss.data['hide_login_btn_on_header'];
	    this._service.nowLoading(false);
	    this._service.updateView();
	    this._service.scrollToTop();
	    this._service.initCurrentComponentDataAsync();
	  }else{
	    this._service.current = null;
	    this._service.nowLoading(false);
	  }
	}
      }
    );

    this._service.flash()._observer = this;
    this._service._router = _router;
    this._service._sign_in_path = "/sign_in";
    this._service._func_update_view = function(){
      let trigger : HTMLElement = document.getElementById('update-view-trigger');
      if(trigger){ trigger.click();} // クリックイベントを発生させ、そこからさらに(click)で登録された関数が呼び出されることで、viewが更新される。
    }
  }

  displayNetworkError(show: boolean){
    // document.getElementById(this._disp_network_error_id).style.display = ((show) ? "block" : "none");
  }
  
  flashBeSet(key: string) {
    this._flash_hidden = false;
    this._flash_sw = !this._flash_sw;
    this._flash_message = this._service.flash().get('message');
    this._flash_error   = this._service.flash().get('error');
    if(this._flash_timer) { clearTimeout(this._flash_timer); }
  }
  
  ngOnInit():any {
  }

  hideFlash(): boolean {
    this._flash_hidden = true;
    return true;
  }
  showFlash(): boolean {
    this._flash_hidden = false;
    return true;
  }
  updateView(){
    // クリックイベントからここが呼ばれると、何もしなくてもviewが更新される
  }


  ngAfterViewInit(){
    this.onResize();    
  }
  
  onResize(){
  }
}
