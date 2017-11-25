require 'json'
class EpgProgramsController < ApplicationController
  set_model EpgProgram

  def self.epgdump(channel_numbers_or_filepaths=[], sec=20)
    cmdfile = Command.epgdump_cmd
    cmdfilepath = cmdfile.path
    case cmdfile
    when Command::GetCommandPathResult::GetSuccess
    when Command::GetCommandPathResult::NotFound
      puts cmdfilepath + " is not found."
      render_data nil
      return
    when Command::GetCommandPathResult::NotExecutable
      puts cmdfilepath + " is not executable."
      render_data nil
      return
    end

    res = {}
    channel_numbers_or_filepaths.map do |ch_or_fpath|
      #  in(ch) out sec
      cmd = "#{cmdfilepath} #{ch_or_fpath} - 20"
      puts cmd
      begin
        json = JSON.parse(`#{cmd}`)
        puts json.length
        json.each do |d|
          # cid = d["id"]
          puts ""
          puts d
          puts ""
        end
        #channel_id = json
        res[ch_or_fpath]=json
      rescue => e
        puts e
      end
    end
    res
  end

  # params: {channels: []}
  def epgdump
    render_data self.class.epgdump(params["in"], params["sec"])
  end

  def self.sample
    {"id"=>"BS_245",
      "transport_stream_id"=>18258,
      "original_network_id"=>4,
      "service_id"=>245,
      "name"=>"Ｊ　ＳＰＯＲＴＳ　４",
      "satelliteinfo"=>{
        "TP"=>"BS21",
        "SLOT"=>2
      },
      "programs"=>[
        {
          "channel"=>"BS_245",
          "title"=>"【2017シーズン一挙放送！】ツール・ド・フランス2017 第7ステージ Cycle*",
          "detail"=>"【開局20周年SP スタートからフィニッシュまで全21ステージ完全生中継！】【トロワ 〜 ニュイ＝サン＝ジョルジュ】解説：栗村修、野寺秀徳実況：サッシャ",
          "extdetail"=>[],
          "start"=>15114942000000,
          "end"=>15115173000000,
          "duration"=>23100,
          "category"=>[
            {
              "large"=>{
                "ja_JP"=>"スポーツ",
                "en"=>"sports"
              },
              "middle"=>{
                "ja_JP"=>"その他",
                "en"=>"Other"
              }
            }
          ],
          "attachinfo"=>[],
          "video"=>{
            "resolution"=>"HD",
            "aspect"=>"16:9"},
          "audio"=>[{"type"=>"デュアルモノ",
              "langcode"=>"jpn_eng",
              "extdesc"=>"主音声副音声"}],
          "freeCA"=>true,
          "event_id"=>30145},
        {"channel"=>"BS_245",
          "title"=>"[無料]アスリートの素顔 presented by ザ・プレミアム・モルツ #4",
          "detail"=>"〜バスケットボール 田中大貴〜練習に取り組むアスリートの姿を追うとともに、アスリートにとっての喜びや、達成感を感じる瞬間、自分にとってのご褒美は何かを探る。",
          "extdetail"=>[],
          "start"=>15115173000000,
          "end"=>15115176000000,
          "duration"=>300,
          "category"=>[{"large"=>{"ja_JP"=>"スポーツ",
                "en"=>"sports"},
              "middle"=>{"ja_JP"=>"その他",
                "en"=>"Other"}}],
          "attachinfo"=>[],
          "video"=>{"resolution"=>"HD",
            "aspect"=>"16:9"},
          "audio"=>[
            {
              "type"=>"ステレオ",
              "langcode"=>"jpn",
              "extdesc"=>"ステレオ"
            }
          ],
          "freeCA"=>false,
          "event_id"=>30146
        },
        {
          "channel"=>"BS_245",
          "title"=>"2017 SUPER GT 第7戦・予選 チャン・インターナショナル・サーキット(タイ)",
          "detail"=>"第7戦・予選 チャン・インターナショナル・サーキット(タイ)解説：光貞秀俊、道上龍実況：赤平 大ピットリポート：高橋二朗、井澤エイミー開催日：2017年10月7日",
          "extdetail"=>[],
          "start"=>15115176000000,
          "end"=>15115248000000,
          "duration"=>7200,
          "category"=>[
            {
              "large"=>{
                "ja_JP"=>"スポーツ",
                "en"=>"sports"
              },
              "middle"=>{
                "ja_JP"=>"モータースポーツ",
                "en"=>"Motor sports"
              }
            }
          ],
          "attachinfo"=>[],
          "video"=>{
            "resolution"=>"HD",
            "aspect"=>"16:9"
          },
          "audio"=>[
            {
              "type"=>"デュアルモノ",
              "langcode"=>"jpn",
              "extdesc"=>"実況解説会場音"
            }
          ],
          "freeCA"=>true,
          "event_id"=>30147
        },
        {
          "channel"=>"BS_245",
          "title"=>"2017 SUPER GT 第7戦・決勝 チャン・インターナショナル・サーキット(タイ)",
          "detail"=>"第7戦・決勝 チャン・インターナショナル・サーキット(タイ)解説：光貞秀俊、道上龍実況：赤平 大ピットリポート：高橋二朗、井澤エイミー開催日：2017年10月8日",
          "extdetail"=>[],
          "start"=>15115248000000,
          "end"=>15115356000000,
          "duration"=>10800,
          "category"=>[{"large"=>{"ja_JP"=>"スポーツ",
                "en"=>"sports"},
              "middle"=>{"ja_JP"=>"モータースポーツ",
                "en"=>"Motor sports"}}],
          "attachinfo"=>[],
          "video"=>{"resolution"=>"HD",
            "aspect"=>"16:9"},
          "audio"=>[{"type"=>"デュアルモノ",
              "langcode"=>"jpn",
              "extdesc"=>"実況解説会場音"}],
          "freeCA"=>true,
          "event_id"=>30148},
        {"channel"=>"BS_245",
          "title"=>"WWE スマックダウン ハイライト 〜アフターバーン〜 【英語版】#953",
          "detail"=>"【英語版】ロウと並ぶWWEのブランド「スマックダウン」。ストーリー性よりも試合内容を重視した番組展開が一番の魅力であるこの番組をハイライトでお届け！",
          "extdetail"=>[],
          "start"=>15115716000000,
          "end"=>15115752000000,
          "duration"=>3600,
          "category"=>[{"large"=>{"ja_JP"=>"スポーツ",
                "en"=>"sports"},
              "middle"=>{"ja_JP"=>"相撲・格闘技",
                "en"=>"Sumo/fighting sports"}}],
          "attachinfo"=>[],
          "video"=>{"resolution"=>"HD",
            "aspect"=>"16:9"},
          "audio"=>[{"type"=>"ステレオ",
              "langcode"=>"eng",
              "extdesc"=>"ステレオ"}],
          "freeCA"=>true,
          "event_id"=>30159},
        {"channel"=>"BS_245",
          "title"=>"新古今HAKA集",
          "detail"=>"[収録HAKA]■オールブラックス　「カマテ」、「カパオパンゴ」■マオリ・オールブラックス■ブルーズ■クルセイダーズ■チーフス",
          "extdetail"=>[],
          "start"=>15115752000000,
          "end"=>15115761000000,
          "duration"=>900,
          "category"=>[{"large"=>{"ja_JP"=>"スポーツ",
                "en"=>"sports"},
              "middle"=>{"ja_JP"=>"その他の球技",
                "en"=>"Other ball games"}}],
          "attachinfo"=>[],
          "video"=>{"resolution"=>"HD",
            "aspect"=>"16:9"},
          "audio"=>[{"type"=>"ステレオ",
              "langcode"=>"jpn",
              "extdesc"=>"ステレオ"}],
          "freeCA"=>true,
          "event_id"=>30160},
        {"channel"=>"BS_245",
          "title"=>"ジレットワールドスポーツ2017 #44 〜世界のスポーツニュース〜",
          "detail"=>"世界各国の膨大なスポーツニュースの中から選りすぐられた情報をお届けするワールドワイドなスポーツ番組。",
          "extdetail"=>[],
          "start"=>15115761000000,
          "end"=>15115779000000,
          "duration"=>1800,
          "category"=>[{"large"=>{"ja_JP"=>"スポーツ",
                "en"=>"sports"},
              "middle"=>{"ja_JP"=>"スポーツニュース",
                "en"=>"Sports news"}}],
          "attachinfo"=>[],
          "video"=>{"resolution"=>"HD",
            "aspect"=>"16:9"},
          "audio"=>[{"type"=>"ステレオ",
              "langcode"=>"jpn",
              "extdesc"=>"ステレオ"}],
          "freeCA"=>true,
          "event_id"=>30161},
        {"channel"=>"BS_245",
          "title"=>"ドローン チャンピオンズリーグ 2017 第4戦 トゥルダ坑跡(ルーマニア)",
          "detail"=>"第4戦 トゥルダ坑跡(ルーマニア)開催日：2017年10月13-14日(現地)ドローンが生み出す音と光と映像は、かつてない感動を呼び込む近未来型エンターテイメント！",
          "extdetail"=>[],
          "start"=>15115779000000,
          "end"=>15115797000000,
          "duration"=>1800,
          "category"=>[{"large"=>{"ja_JP"=>"スポーツ",
                "en"=>"sports"},
              "middle"=>{"ja_JP"=>"モータースポーツ",
                "en"=>"Motor sports"}}],
          "attachinfo"=>[],
          "video"=>{"resolution"=>"HD",
            "aspect"=>"16:9"},
          "audio"=>[{"type"=>"ステレオ",
              "langcode"=>"jpn",
              "extdesc"=>"ステレオ"}],
          "freeCA"=>true,
          "event_id"=>30162},
        {"channel"=>"BS_245",
          "title"=>"【2017シーズン一挙放送！】ツール・ド・フランス2017 第8ステージ Cycle*",
          "detail"=>"【開局20周年SP スタートからフィニッシュまで全21ステージ完全生中継！】【ドール 〜 スタスィヨン・デ・ルッス】解説：狩野智也、野寺秀徳ナビゲーター：栗村修",
          "extdetail"=>[],
          "start"=>15115797000000,
          "end"=>15116013000000,
          "duration"=>21600,
          "category"=>[{"large"=>{"ja_JP"=>"スポーツ",
                "en"=>"sports"},
              "middle"=>{"ja_JP"=>"その他",
                "en"=>"Other"}}],
          "attachinfo"=>[],
          "video"=>{"resolution"=>"HD",
            "aspect"=>"16:9"},
          "audio"=>[{"type"=>"デュアルモノ",
              "langcode"=>"jpn_eng",
              "extdesc"=>"主音声副音声"}],
          "freeCA"=>true,
          "event_id"=>30163},
        {"channel"=>"BS_245",
          "title"=>"我らワールドのサイクルロードレース観戦塾2017 #7 Cycle*",
          "detail"=>"栗村修、サッシャの“我らファミリー”がお送りするサイクルロードレース情報番組「我らワールド」！",
          "extdetail"=>[],
          "start"=>15116013000000,
          "end"=>15116031000000,
          "duration"=>1800,
          "category"=>[{"large"=>{"ja_JP"=>"スポーツ",
                "en"=>"sports"},
              "middle"=>{"ja_JP"=>"その他",
                "en"=>"Other"}}],
          "attachinfo"=>[],
          "video"=>{"resolution"=>"HD",
            "aspect"=>"16:9"},
          "audio"=>[{"type"=>"ステレオ",
              "langcode"=>"jpn",
              "extdesc"=>"ステレオ"}],
          "freeCA"=>true,
          "event_id"=>30164},
        {"channel"=>"BS_245",
          "title"=>"Cycle*2017 コンタドール引退特別番組 〜エル・ピストレロの軌跡〜",
          "detail"=>"【あなたが選ぶツール・ド・フランス2007＆2009 ベストステージ】第1位　ツール・ド・フランス2009 第15ステージ",
          "extdetail"=>[],
          "start"=>15116031000000,
          "end"=>15116211000000,
          "duration"=>18000,
          "category"=>[{"large"=>{"ja_JP"=>"スポーツ",
                "en"=>"sports"},
              "middle"=>{"ja_JP"=>"その他",
                "en"=>"Other"}}],
          "attachinfo"=>[],
          "video"=>{"resolution"=>"HD",
            "aspect"=>"16:9"},
          "audio"=>[{"type"=>"デュアルモノ",
              "langcode"=>"jpn_fra",
              "extdesc"=>"主音声副音声"}],
          "freeCA"=>true,
          "event_id"=>30165},
        {"channel"=>"BS_245",
          "title"=>"Cycle*2017 ジャパンカップ クリテリウム",
          "detail"=>"【現地】解説：栗村修実況：サッシャ開催日：2017年10月21日会場：栃木県宇都宮市大通り周回コース",
          "extdetail"=>[],
          "start"=>15116211000000,
          "end"=>15116283000000,
          "duration"=>7200,
          "category"=>[{"large"=>{"ja_JP"=>"スポーツ",
                "en"=>"sports"},
              "middle"=>{"ja_JP"=>"その他",
                "en"=>"Other"}}],
          "attachinfo"=>[],
          "video"=>{"resolution"=>"HD",
            "aspect"=>"16:9"},
          "audio"=>[{"type"=>"ステレオ",
              "langcode"=>"jpn",
              "extdesc"=>"ステレオ"}],
          "freeCA"=>true,
          "event_id"=>30166},
        {"channel"=>"BS_245",
          "title"=>"サッカー プレミアリーグ プレビューショー #13",
          "detail"=>"欧州サッカーの中でも人気、実力ともにトップランクに挙げられるイングランド プレミアリーグの週末の見どころを紹介。試合を前にプレビューショーで最新情報をチェック！",
          "extdetail"=>[],
          "start"=>15116283000000,
          "end"=>15116301000000,
          "duration"=>1800,
          "category"=>[{"large"=>{"ja_JP"=>"スポーツ",
                "en"=>"sports"},
              "middle"=>{"ja_JP"=>"サッカー",
                "en"=>"soccer"}}],
          "attachinfo"=>[],
          "video"=>{"resolution"=>"HD",
            "aspect"=>"16:9"},
          "audio"=>[{"type"=>"デュアルモノ",
              "langcode"=>"jpn_eng",
              "extdesc"=>"主音声副音声"}],
          "freeCA"=>true,
          "event_id"=>30167},
        {"channel"=>"BS_245",
          "title"=>"[生]サッカー プレミアリーグ 第13節-5 リヴァプール×チェルシー",
          "detail"=>"リヴァプール×チェルシー解説：戸田和幸 実況：倉敷保雄開催日：2017年11月25日(現地)会場：アンフィールド, リヴァプール",
          "extdetail"=>[],
          "start"=>15116301000000,
          "end"=>15116400000000,
          "duration"=>9900,
          "category"=>[{"large"=>{"ja_JP"=>"スポーツ",
                "en"=>"sports"},
              "middle"=>{"ja_JP"=>"サッカー",
                "en"=>"soccer"}}],
          "attachinfo"=>[],
          "video"=>{"resolution"=>"HD",
            "aspect"=>"16:9"},
          "audio"=>[{"type"=>"デュアルモノ",
              "langcode"=>"jpn_eng",
              "extdesc"=>"主音声副音声"}],
          "freeCA"=>true,
          "event_id"=>30168},
        {"channel"=>"BS_245",
          "title"=>"【新シリーズ放送開始！！】桑田泉のゴルフアカデミー シーズン5 #1",
          "detail"=>"ボールを見るな！ダフれ！手打ちしろ！　桑田泉のクォーター理論でゴルフが変わる桑田流壁切りゴルフ!!",
          "extdetail"=>[],
          "start"=>15116400000000,
          "end"=>15116418000000,
          "duration"=>1800,
          "category"=>[{"large"=>{"ja_JP"=>"スポーツ",
                "en"=>"sports"},
              "middle"=>{"ja_JP"=>"ゴルフ",
                "en"=>"Golf"}}],
          "attachinfo"=>[],
          "video"=>{"resolution"=>"HD",
            "aspect"=>"16:9"},
          "audio"=>[{"type"=>"ステレオ",
              "langcode"=>"jpn",
              "extdesc"=>"ステレオ"}],
          "freeCA"=>true,
          "event_id"=>30169},
        {"channel"=>"BS_245",
          "title"=>"ロシアフィギュアスケート選手権2017 男子シングル",
          "detail"=>"〜男子シングル〜解説：岡部由起子実況：小林千鶴開催日：2016年12月22-23日(現地)",
          "extdetail"=>[],
          "start"=>15116418000000,
          "end"=>15116490000000,
          "duration"=>7200,
          "category"=>[{"large"=>{"ja_JP"=>"スポーツ",
                "en"=>"sports"},
              "middle"=>{"ja_JP"=>"マリン・ウインタースポーツ",
                "en"=>"Marine/winter sports"}}],
          "attachinfo"=>[],
          "video"=>{"resolution"=>"HD",
            "aspect"=>"16:9"},
          "audio"=>[{"type"=>"デュアルモノ",
              "langcode"=>"jpn",
              "extdesc"=>"実況解説会場音"}],
          "freeCA"=>true,
          "event_id"=>30170},
        {"channel"=>"BS_245",
          "title"=>"小塚崇彦のフィギュアスケート・ラボ2017　ペア 前編",
          "detail"=>"ゲスト：岡部由起子（ISU技術委員）新しい発見がきっとある。美しいスケーティングと独自のスケート理論を持つ小塚崇彦がカップル競技について徹底研究。",
          "extdetail"=>[],
          "start"=>15116490000000,
          "end"=>15116508000000,
          "duration"=>1800,
          "category"=>[{"large"=>{"ja_JP"=>"スポーツ",
                "en"=>"sports"},
              "middle"=>{"ja_JP"=>"マリン・ウインタースポーツ",
                "en"=>"Marine/winter sports"}}],
          "attachinfo"=>[],
          "video"=>{"resolution"=>"HD",
            "aspect"=>"16:9"},
          "audio"=>[{"type"=>"ステレオ",
              "langcode"=>"jpn",
              "extdesc"=>"ステレオ"}],
          "freeCA"=>true,
          "event_id"=>30171},
        {"channel"=>"BS_245",
          "title"=>"小塚崇彦のフィギュアスケート・ラボ2017　ペア 後編",
          "detail"=>"ゲスト：岡部由起子（ISU技術委員）新しい発見がきっとある。美しいスケーティングと独自のスケート理論を持つ小塚崇彦がカップル競技について徹底研究。",
          "extdetail"=>[],
          "start"=>15116508000000,
          "end"=>15116526000000,
          "duration"=>1800,
          "category"=>[{"large"=>{"ja_JP"=>"スポーツ",
                "en"=>"sports"},
              "middle"=>{"ja_JP"=>"マリン・ウインタースポーツ",
                "en"=>"Marine/winter sports"}}],
          "attachinfo"=>[],
          "video"=>{"resolution"=>"HD",
            "aspect"=>"16:9"},
          "audio"=>[{"type"=>"ステレオ",
              "langcode"=>"jpn",
              "extdesc"=>"ステレオ"}],
          "freeCA"=>true,
          "event_id"=>30172},
        {"channel"=>"BS_245",
          "title"=>"ロシアフィギュアスケート選手権2017 ペア",
          "detail"=>"〜ペア〜解説：岡部由起子　実況：小林千鶴開催日：2016年12月23-24日(現地)",
          "extdetail"=>[],
          "start"=>15116526000000,
          "end"=>15116598000000,
          "duration"=>7200,
          "category"=>[{"large"=>{"ja_JP"=>"スポーツ",
                "en"=>"sports"},
              "middle"=>{"ja_JP"=>"マリン・ウインタースポーツ",
                "en"=>"Marine/winter sports"}}],
          "attachinfo"=>[],
          "video"=>{"resolution"=>"HD",
            "aspect"=>"16:9"},
          "audio"=>[{"type"=>"デュアルモノ",
              "langcode"=>"jpn",
              "extdesc"=>"実況解説会場音"}],
          "freeCA"=>true,
          "event_id"=>30173},
        {"channel"=>"BS_245",
          "title"=>"ロシアフィギュアスケート選手権2017 女子シングル",
          "detail"=>"〜女子シングル〜解説：岡部由起子実況：小林千鶴開催日：2016年12月23-24日(現地)",
          "extdetail"=>[],
          "start"=>15116598000000,
          "end"=>15116670000000,
          "duration"=>7200,
          "category"=>[{"large"=>{"ja_JP"=>"スポーツ",
                "en"=>"sports"},
              "middle"=>{"ja_JP"=>"マリン・ウインタースポーツ",
                "en"=>"Marine/winter sports"}}],
          "attachinfo"=>[],
          "video"=>{"resolution"=>"HD",
            "aspect"=>"16:9"},
          "audio"=>[{"type"=>"デュアルモノ",
              "langcode"=>"jpn",
              "extdesc"=>"実況解説会場音"}],
          "freeCA"=>true,
          "event_id"=>30174},
        {"channel"=>"BS_245",
          "title"=>"フィギュアスケーターのオアシス KENJIの部屋【深瀬理香子＆立野在 #3】",
          "detail"=>"深瀬理香子＆立野在組が登場。14-15シーズンからカップルを組み全日本ジュニア選手権を3連覇。今シーズンよりシニアに参戦する、期待のアイスダンスカップルの素顔に迫る!",
          "extdetail"=>[],
          "start"=>15116670000000,
          "end"=>15116688000000,
          "duration"=>1800,
          "category"=>[
            {
              "large"=>{
                "ja_JP"=>"スポーツ",
                "en"=>"sports"
              },
              "middle"=>{
                "ja_JP"=>"マリン・ウインタースポーツ",
                "en"=>"Marine/winter sports"
              }
            }
          ],
          "attachinfo"=>[],
          "video"=>{
            "resolution"=>"HD",
            "aspect"=>"16:9"
          },
          "audio"=>[
            {
              "type"=>"ステレオ",
              "langcode"=>"jpn",
              "extdesc"=>"ステレオ"
            }
          ],
          "freeCA"=>true,
          "event_id"=>30175
        },
        {
          "channel"=>"BS_245",
          "title"=>"【2017シーズン一挙放送！】ツール・ド・フランス2017 第9ステージ Cycle*",
          "detail"=>"【スタートからフィニッシュまで全21ステージ完全生中継！】【ナンテュア 〜 シャンベリー】ゲスト：ルーク篁(元聖飢魔II)解説：飯島誠実況：谷口廣明",
          "extdetail"=>[],
          "start"=>15116688000000,
          "end"=>15116922000000,
          "duration"=>23400,
          "category"=>[
            {
              "large"=>{
                "ja_JP"=>"スポーツ",
                "en"=>"sports"
              },
              "middle"=>{"ja_JP"=>"その他",
                "en"=>"Other"
              }
            }
          ],
          "attachinfo"=>[],
          "video"=>{
            "resolution"=>"HD",
            "aspect"=>"16:9"
          },
          "audio"=> [
            {
              "type"=>"デュアルモノ",
              "langcode"=>"jpn_eng",
              "extdesc"=>"主音声副音声"
            }
          ],
          "freeCA"=>true,
          "event_id"=>30176
        }
      ]
    }
  end

  def self.permitted_params
    [:start_time, :stop_time, :channel_id, :title, :desc, :epg_program_category_id]
  end
end
