import {Injectable} from '@angular/core'
import {FlashService} from './flash.service'
import {API} from './models/api'
import {RemoteAPI} from './models/remote.api'
import {Router} from '@angular/router'

/****************************
 * このサービスを作成した理由
 ****************************
 *
 * 当初は、それぞれのサービスをbootstrapに渡したかったのだが、サービスがコンポーネントのサービスに渡るときにインターフェースとして渡すことができなかったため。
 * 
 * 以下の書き方は実行時にエラー
 * export class XxxComponent {
 *   constructor(api: API) {}  // APIはインターフェース名なのでダメ
 * }
 *
 * 以下のようにすればOK
 * export class XxxComponent {
 *   constructor(api: LocalStorageAPI)) {} // LocalStorageAPIはクラス名なのでOK
 * }
 * 
 * Javaなど一般的なオブジェクト指向言語であれば、コンストラクタの引数の型としてインターフェースを指定するのは普通だが、Angular2の場合だと（コンポーネントのコンストラクタだけかもしれないが）ダメのようだ。
 * 
 * サーバに接続するためのAPIについては、まだサーバの実装が存在しないので、APIの定義だけ先行して行い、ある程度動作させたかった。なので、サーバが実装された後でAPIの実装も切り替えられるようにしたかった。
 */

export interface ExternalUserProfile {
  name? : string;
  email? : string;
  picture_url? : string;
}

export interface Current {
  initData() : void;
}

@Injectable()
export class AllService {
  private loading_closer_id : number = null;
  public now_loading_id = "now-loading";
  public now_loading_flag = false;

  public current : Current = null;
  public viewport_user_scalable : boolean = true;
  
  constructor () {
  }

  public _router : Router = null;
  public _sign_in_path : string = null;
  public _func_update_view  = function(){};
  private _flash: FlashService     = new FlashService();
  private _api: API                = new RemoteAPI();
  //private _api: API                = new LocalStorageAPI();


  public nowLoading(shown: boolean){
    if(this.now_loading_flag == shown){ return; } // 変化がないなら何もしない
    let e = document.getElementById(this.now_loading_id);
    if(e){
      e.style.width = ((shown) ? "100%" : "0");
      this.now_loading_flag = shown;
    }
  }
  
  public flash():   FlashService   { return this._flash;   }
  public api():     API { return this._api; }

  public updateView() : void {
    this._func_update_view();
  }
  public scrollToTop() : void {
    window.scrollTo(0, 0) ;
  }

  public initCurrentComponentDataAsync(){
    if(this.current){
      this.nowLoading(true);
      setTimeout(() => {
	this.current.initData();
	this.nowLoading(false);
      },250);
    }
  }
  
  public updateViewportUserScalable(){
    let yesno : boolean = this.viewport_user_scalable;
    let meta : HTMLElement = document.getElementById('viewport-user-scalable');
    if(meta){
      meta.setAttribute('content', 'user-scalable=' + (yesno ? 'yes' : 'no'));
    }
  }
}
