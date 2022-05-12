%%===================================
%%  AUTO GENERATE BY CFG_EXPORTER
%%===================================
-ifndef(CONFIG_ATTR_HRL).
-define(CONFIG_ATTR_HRL, true).

-record(config_attr, {
    id,                                                                         % 单位类型
    name,                                                                       % 单位名称
    sign,                                                                       % 标记
    sign_color,                                                                 % 标记颜色
    word_color,                                                                 % 字体颜色
    module,                                                                     % 模块名
    atk,                                                                        % 攻击力
    def,                                                                        % 防御力
    mov,                                                                        % 移动速度trunc(1000/mov*1000)
    spd,                                                                        % 攻击速度trunc(1000/mov*1000)
    hp,                                                                         % 初始血量
    cri,                                                                        % 暴击率
    guard_range,                                                                % 警戒范围
    atk_range,                                                                  % 攻击范围
    args                                                                        % 参数
}).


-endif.
