#include <ctime>
#include <iostream>

#include "MyStrategy.h"

#include "Haskell_stub.h"

using namespace model;

MyStrategy::MyStrategy() {
    allTime = std::clock();
    isStrategyComputed = false;
}

MyStrategy::~MyStrategy() {
    double seconds = double(std::clock()-allTime)/CLOCKS_PER_SEC;
    std::cout << "Time: " << seconds << "s."<< std::endl;
}

std::string MyStrategy::custom_rendering() { return "";}

template<class T>
void print(T a, int n){
    for (int i = 0; i < n; ++i)
        std::cout << a[i] << " ";
}

void MyStrategy::act(const Robot& me0, const Rules& rules, const Game& game, Action& action) {
    if (game.current_tick == 0 && !isStrategyComputed){
        stored  = std::vector<double>(100);
        stored0 = std::vector<double>(100);
        std::cout << "INI" << std::endl;
    }

    Robot mate;
    for(const auto& r: game.robots){
        if (r.is_teammate && r.id!=me0.id)
            mate = r;
    }

    if (me0.id < mate.id){
        //std::cout << game.current_tick << ": ";
        std::vector<double> showS = stored;
        if(isStrategyComputed){
            showS = stored0;
        }
        //std::cout << 
        //    showS[8]  - game.ball.x << " " <<
        //    showS[9]  - game.ball.y << " " <<
        //    showS[10] - game.ball.z << " " << 
        //    showS[11] - game.ball.velocity_x << " " <<
        //    showS[12] - game.ball.velocity_y << " " <<
        //    showS[13] - game.ball.velocity_z << " " << 
        //    //showS[14] << " " <<
        //    //showS[15] << " " <<
        //    //showS[16] << " " << 
        //    //showS[14] - me0.x << " " <<
        //    //showS[15] - me0.y << " " <<
        //    //showS[16] - me0.z << " " << 
        //    std::endl;
    }


    if (!isStrategyComputed){
        Player iAm;
        Player enemy;
        for(const auto& p: game.players){
            if (p.me)
                iAm = p;
            if (!p.me)
                enemy = p;
        }

        Robot me;
        Robot mate;
        Robot eRobot;
        Robot eRobot0;
        bool isESet = false;
        for(const auto& r: game.robots){
            if (r.is_teammate && r.id!=me0.id)
                mate = r;
            if (enemy.id == r.player_id && !isESet){
                eRobot = r;
                isESet = true;
            }
            if (enemy.id == r.player_id && isESet)
                eRobot0 = r;
        }
        if (mate.id < me0.id){
            me = mate;
            mate = me0;
        }else{
            me = me0;
        }

        //std::cout << game.current_tick << ": " 
        //          << game.ball.x << " " 
        //          << game.ball.y << " " 
        //          << game.ball.z << " "
        //          << std::endl; 


        double* out = static_cast<double*>(
                haskellAct(
                    me.id, me.is_teammate, me.x, me.y, me.z,
                    me.velocity_x, me.velocity_y, me.velocity_z, me.radius,
                    me.touch, me.touch_normal_x, me.touch_normal_y, me.touch_normal_z,
                    mate.id, mate.is_teammate, mate.x, mate.y, mate.z,
                    mate.velocity_x, mate.velocity_y, mate.velocity_z, mate.radius,
                    mate.touch, mate.touch_normal_x, mate.touch_normal_y, mate.touch_normal_z,
                    eRobot.id, eRobot.is_teammate, eRobot.x, eRobot.y, eRobot.z,
                    eRobot.velocity_x, eRobot.velocity_y, eRobot.velocity_z, eRobot.radius,
                    eRobot.touch, eRobot.touch_normal_x, eRobot.touch_normal_y, eRobot.touch_normal_z,
                    eRobot0.id, eRobot0.is_teammate, eRobot0.x, eRobot0.y, eRobot0.z,
                    eRobot0.velocity_x, eRobot0.velocity_y, eRobot0.velocity_z, eRobot0.radius,
                    eRobot0.touch, eRobot0.touch_normal_x, eRobot0.touch_normal_y, eRobot0.touch_normal_z,
                    game.ball.x, game.ball.y, game.ball.z,
                    game.ball.velocity_x, game.ball.velocity_y, game.ball.velocity_z, game.ball.radius,
                    game.current_tick, iAm.score, enemy.score, stored.data()
                    ));

        std::copy(stored.begin(), stored.end(), stored0.begin());
        std::copy(out, out+4+4+6+3, stored.begin());

        isStrategyComputed = true;
    }
    else{
        isStrategyComputed = false;
    }


    if (me0.id < mate.id){
        action.target_velocity_x = stored[0];
        action.target_velocity_y = stored[1];
        action.target_velocity_z = stored[2];
        action.jump_speed        = stored[3];
    }else{
        action.target_velocity_x = stored[4];
        action.target_velocity_y = stored[5];
        action.target_velocity_z = stored[6];
        action.jump_speed        = stored[7];
    }

}

