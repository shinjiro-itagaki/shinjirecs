import {API} from './models/api'

export class NetworkObserver {
  private _api : API;
  private _network_observer_id : number = null;
  private _now_network_error : boolean = false;
  private _network_error_counter : number = 0;
  private _interval : number = 1000;
  
  public onErrorOccured : () => void = null;
  public onReconnected  : () => void = null;
  
  constructor(private api: API, private interval? : number) {
    this._api = api;
    if(interval){
      this._interval = interval;
    }
  }

  get now_error() : boolean {
    return this._now_network_error;
  }
  
  public onError(){
    this._now_network_error = true;
    this._network_error_counter = (this._network_error_counter + 1) % 100;

    if(!this._network_observer_id){ // オブザーバが起動していない場合
      if(this.onErrorOccured){
	this.onErrorOccured();
      }
      
      // ネットワークが回復したかどうか監視するオブザーバを起動      
      this._network_observer_id = window.setInterval(() => {
	let old_network_counter = this._network_error_counter;
	if(old_network_counter == this._network_error_counter){
	  // カウンターの値が変化していないのでネットワークが回復したはず
	  if(this.onReconnected){
	    this.onReconnected();
	  }	  
	  clearInterval(this._network_observer_id);
	  this._network_observer_id = null;
	}
      }, this._interval);
    }    
  }
}
