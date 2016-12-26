/// <reference path="../src/lib/jquery.d.ts" />
/// <reference path="./../config/config.d.ts" />
/// <reference path="../src/lib/ga.d.ts" />
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
    <div id="{{_disp_network_error_id}}">ネットワークエラーが発生しています</div>
    <div id="now-loading"></div>
    <div (window:resize)="onResize();">
    <div id="update-view-trigger" (click)="updateView()" style="display:none;" class="click-btn"></div>
    <header [ngClass]="{header_closed: _hide_header}">
      <div id="flash-area" class="flash-area click-btn" (click)="hideFlash()" [ngClass]="{hidden: _flash_hidden, shown0: (!_flash_hidden && _flash_sw), shown1: (!_flash_hidden && !_flash_sw)}">
      <div class="flash-area-wrapper">
        <span [ngClass]="{hidden: (_flash_message == null)}" class="flash message">{{ _flash_message}}</span>
        <span [ngClass]="{hidden: (_flash_error   == null)}" class="flash error"  >{{ _flash_error  }}</span>
      </div>
      </div>    
    <div *ngIf="!_hide_header" class="header-main">
    <h1><a [routerLink]="['']"><img src="./src/images/common/logo_200x76.png"></a></h1>
    <nav><ul class="header-left-menu">
      <li *ngIf="_service.session().isLogged()" ><a role="menuitem" [routerLink]="['/work']"         style="background-image: url(./src/images/common/icon_navi01.png);" class="budokai">武道会</a></li>
      <li *ngIf="_service.session().isLogged()" ><a role="menuitem" [routerLink]="['/show_ranking']" style="background-image: url(./src/images/common/icon_navi02.png);">ランキング</a></li>
      <li *ngIf="_service.session().isLogged()" ><a role="menuitem" [routerLink]="['/my_page_home']" style="background-image: url(./src/images/common/icon_navi03.png)">マイページ</a></li>
    </ul></nav>
    <nav class="header-right-menu">
      <li class="header-right-menu-text"><a [routerLink]="['/knowhow']">利用方法</a>　|</li>
      <li class="header-right-menu-text"><a [routerLink]="['/faq']">FAQ</a></li>
      <li class="facobooklogin" *ngIf="!_hide_login_btn_on_header && !_service.session().isLogged()" ><a role="menuitem" (click)="_service.login()" class="click-btn"><span class="text">ログイン</span></a></li>
      <li class="thinklivebutton" *ngIf="_service.session().isLogged()" ><a role="menuitem" href="${thinkingLiveHref}" ><span class="table_"><span class="cell icon" style="background-image: url(./src/images/common/icon_navi_live.png);"></span><span class="cell text">思考ライブ！</span></span></a></li>
    </nav>

    <nav class="sp">
    <div class="menu-area">
    <div class="btn-group">
    <button class="btn btn-default dropdown-toggle" aria-label="Justify" type="button" data-toggle="dropdown" aria-expanded="false">
    <span aria-hidden="true"><img src="./src/images/icon_header_menu.png" style="width: 30px;"></span>
    </button>
    <ul *ngIf="_service.session().isLogged()" class="dropdown-menu dropdown-menu-custom" role="menu" [style.width]="screen_size_width" >
    <li role="presentation" class="close_img"><img src="./src/images/icon_close.png" style="width: 30px"></li>
    <li role="presentation" class="top_img"><img src="./src/images/common/logo_200x76.png" style="width: 150px;"></li>
    <li role="presentation">
      <a role="menuitem" tabindex="-1" [routerLink]="['/work']">
      <img src="./src/images/common/icon_navi01.png" class="left-img">
      <span class="center-text">武道会</span>
      <img src="./src/images/icon_ar_right_w.png" class="right-img">
      </a>
    </li>
    <li role="presentation">
      <a role="menuitem" tabindex="-1" [routerLink]="['/show_ranking']">
       <img src="./src/images/common/icon_navi02.png" class="left-img">
       <span class="center-text">ランキング</span>
       <img src="./src/images/icon_ar_right_w.png" class="right-img">
     </a>
    </li>
    <li role="presentation">
      <a role="menuitem" tabindex="-1" [routerLink]="['/my_page_home']">
       <img src="./src/images/common/icon_navi03.png" class="left-img">
       <span class="center-text">マイページ</span>
       <img src="./src/images/icon_ar_right_w.png" class="right-img">     
      </a>
    </li>
    <li role="presentation" class="howto">
      <a role="menuitem" tabindex="-1" [routerLink]="['/knowhow']">
       <span>利用方法</span>
       <img src="./src/images/icon_ar_right_w.png" class="right-img">
      </a>
    </li>
    <li role="presentation" class="faq" >
      <a role="menuitem" tabindex="-1" [routerLink]="['/faq']">
       <span>FAQ</span>
       <img src="./src/images/icon_ar_right_w.png" class="right-img">
      </a>
    </li>
    <li role="presentation" class="live"><div class="thinklivebutton"><a role="menuitem" tabindex="-1" class="click-btn" href="${thinkingLiveHref}"><span class="table_"><span class="cell icon" style="background-image: url(./src/images/common/icon_navi_live.png);"></span><span class="cell text">思考ライブ！</span></span></a></div></li>
    </ul>
    <ul *ngIf="!_service.session().isLogged()" class="dropdown-menu dropdown-menu-custom" role="menu" [style.width]="screen_size_width">
    <li role="presentation" class="close_img"><img src="./src/images/icon_close.png" style="width: 30px"></li>
    <li role="presentation" class="top_img" style="border: none;"><img src="./src/images/common/logo_200x76.png" style="width: 150px;"></li>
    <li role="presentation" class="facebook"><div class="facobooklogin" ><a role="menuitem" tabindex="-1" (click)="_service.login()" class="click-btn"><span class="text">Facebookで今すぐログイン</span></a></div><br><div class="facebooklogin_disc">本サービスのご利用はFacebookアカウントが必要です</div></li>
    <li role="presentation">
      <a role="menuitem" tabindex="-1" [routerLink]="['/knowhow']">
       <span>利用方法</span>
       <img src="./src/images/icon_ar_right_w.png" class="right-img">
      </a>
    </li>
    <li role="presentation">
      <a role="menuitem" tabindex="-1" [routerLink]="['/faq']">
       <span>FAQ</span>
       <img src="./src/images/icon_ar_right_w.png" class="right-img">
      </a>
    </li>
    </ul>
    </div>
    </div>
    <span class="header-right-icon-menu">
    <a class="menuitem" style="background-image: url(./src/images/common/icon_navi_live.png);" href="${thinkingLiveHref}"></a>
    <a class="menuitem" style="background-image: url(./src/images/common/icon_navi02.png);" [routerLink]="['/show_ranking']"></a>
    </span>
    </nav>
    

    </div>
    </header>
    <div id="main" [ngClass]="{header_closed: _hide_header}">
    <router-outlet></router-outlet>
    </div>
    <footer>
    <a [routerLink]="['']"><img class="footerlogo" src="./src/images/common/logo_200x76.png"></a>
    <nav>
    <ul>
    <li><a href="mailto: {{mailto}}">お問い合わせ</a>　|</li>
    <li><a [routerLink]="['/terms']">利用規約</a>　|</li>
    <li><a href="https://www.bbt757.com/privacy.html">プライバシーポリシー</a></li>
    </ul>      
    </nav>
    <p><small class="copyright">Copyright &copy; 2016 Business Breakthrough Inc. All Rights reserved. </small></p>
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
  mailto: string = Config.contact.mailto;

  private _network_observer : NetworkObserver = null;
  private _disp_network_error_id = 'now-network-error';

  private _old_ga_pathname = '';
  
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
	    if(path && path != this._old_ga_pathname){ // 二重送信を防止
              ga('send','pageview',path); // この処理を最優先にするため最初に実行する
	      this._old_ga_pathname = window.location.pathname;
	    }
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
    document.getElementById(this._disp_network_error_id).style.display = ((show) ? "block" : "none");
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
    this.screen_size_width = $(window).width();

    let divW: number = Config.responsive_width; // pcかスマホか判別する基準となる幅
    let realW: number = Math.min(window.innerWidth,window.screen.width); // 表示するウインドウと、デバイスの最大幅のどちらか短い方を基準にviewportのwidthを設定。スマホだと、デバイス幅は320pxなのに、windowの幅が960pxなどと出力されることがあるので
    let viewportW : number = realW > divW ? Config.pc_width : Config.mb_width;
    let scale : number = realW / viewportW;
    let desc : string = "width=device-width,initial-scale=" + scale + ",minimum-scale=1.0,maximum-scale=1.0,user-scalable=no";
    let meta : HTMLElement = document.getElementById('viewport-config');
    meta.setAttribute("content",desc);
  }
}
