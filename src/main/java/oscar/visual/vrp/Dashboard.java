package oscar.visual.vrp;

import oscar.cbls.routing.visual.VisualDebug;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.concurrent.Semaphore;

/*******************************************************************************
 * This file is part of OscaR (Scala in OR).
 *
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/gpl-3.0.html
 ******************************************************************************/

/*******************************************************************************
 * Contributors:
 *     This code has been initially developed by Ghilain Florent.
 ******************************************************************************/


public class Dashboard extends JPanel {
    JButton nextIte = null;
    public JButton start = null;
    JScrollPane routeScroll = null;
    JTextArea route = null;
    public JComboBox heuristic = null;
    public JTextField nbNodes = null;
    public JTextField nbVehicle = null;
    public JComboBox instances = null;
    JButton makeInstance = null;
    JButton resetInstance = null;
    public JComboBox neighborhood = null;
    public JTextField klimited = null;

    JCheckBox writeRoute = null;
    public Boolean pause = true;
    Boolean iteration = false;
    Semaphore semPause = new Semaphore(0);
    public Boolean firstIte = true;

    public Dashboard(){

        setBackground(Color.white);
        setLayout(new FlowLayout());
        setPointsAndVehicles();
        setHeuristic();
        setInstances();

        setResetInstance();
        setStartButton();
        setNextIteButton();
        setLogRoute();
        setRouteLabel();

        setNeighborhood();

     }

    public void setNextIteButton(){
        nextIte = new JButton("Next iteration");
        nextIte.setBackground(Color.white);
        nextIte.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                if(firstIte){
                    new Thread(new VisualDebug.Search()).start();
                    firstIte = false;
                }
                if(pause){
                    iteration = true;
                    unlock();
                }
            }
        });
        add(nextIte);
    }

    public void setNeighborhood(){

        //TODO ajouter pénalité
        JLabel neighborLabel = new JLabel();
        neighborLabel.setText("Neighbor : ");
        neighborLabel.setBackground(Color.white);
        neighborhood = new JComboBox(new String[]{"OnePointMove","ReinsertPoint","RemovePoint","Swap","ThreeOptA"
        ,"ThreeOptB","ThreeOptC","TwoOpt"});
        neighborhood.setBackground(Color.white);

        JLabel klimitedLabel = new JLabel();
        klimitedLabel.setText("K-limited : ");
        klimitedLabel.setBackground(Color.white);
        klimited = new JTextField();
        klimited.setText("20");
        klimited.setPreferredSize(new Dimension(35,25));
        klimited.setBackground(Color.white);

        JPanel neighborhoodPanel = new JPanel();

        neighborhoodPanel.add(neighborLabel);
        neighborhoodPanel.add(neighborhood);
        neighborhoodPanel.add(klimitedLabel);
        neighborhoodPanel.add(klimited);

        neighborhoodPanel.setBackground(Color.white);
        neighborhoodPanel.setBorder(BorderFactory.createTitledBorder("Neighborhood Options"));
        neighborhoodPanel.setPreferredSize(new Dimension(400,100));
        add(neighborhoodPanel);
    }

    public void setResetInstance(){
        resetInstance = new JButton("Reset instance");
        resetInstance.setBackground(Color.white);
        resetInstance.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                //firstIte = true;
                VisualDebug.initModel(true);

            }
        });
        add(resetInstance);
    }


    public void lock(){
        try {
            semPause.acquire();
        }catch(Exception e) {
            e.printStackTrace();
        }
    }

    public void unlock() {
        semPause.release();
    }

    public void setRouteLabel(){

        route = new JTextArea("Route:\n");
        route.setSize(new Dimension(500, 100));

        routeScroll = new JScrollPane();
        routeScroll.setPreferredSize(new Dimension(500, 100));
        routeScroll.getViewport().add(route);
        add(routeScroll);
    }

    public void updateRouteLabel(String line){
       route.append(line + "\n");
     }

    public void setStartButton(){
        start = new JButton("Start");
        start.setBackground(Color.white);
        start.setMaximumSize(new Dimension(100, 50));
        start.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                if(firstIte){
                    new Thread(new VisualDebug.Search()).start();
                    firstIte = false;
                }
                pause = !pause;
                if (pause)
                    start.setText("Restart");
                else {
                    iteration = false;
                    start.setText("Pause");
                    unlock();
                }
            }
        });
        add(start);
    }

    public void setLogRoute(){
        writeRoute = new JCheckBox();
        writeRoute.setToolTipText("Write routes (debug mode)");
        writeRoute.setBackground(Color.white);
        add(writeRoute);
    }

    public void setHeuristic(){
        heuristic = new JComboBox(new String[]{"RandomNeighbor","NearestNeighbor"});
        heuristic.setName("Heuristic");
        heuristic.setBackground(Color.white);
        JPanel heuristicPanel = new JPanel();
        heuristicPanel.setBackground(Color.white);
        heuristicPanel.setBorder(BorderFactory.createTitledBorder("Heuristic"));
        heuristicPanel.add(heuristic);

        add(heuristicPanel);
    }

    public void setPointsAndVehicles(){
        JLabel nodes = new JLabel("Points :");
        JLabel vehicles = new JLabel("Vehicles :");
        nbNodes = new JTextField();
        nbNodes.setPreferredSize(new Dimension(50, 25));
        nbNodes.setText("10");
        nbVehicle = new JTextField();
        nbVehicle.setPreferredSize(new Dimension(25, 25));
        nbVehicle.setText("1");

        JPanel nbInput = new JPanel();
        nbInput.setBackground(Color.white);
        nbInput.setBorder(BorderFactory.createTitledBorder("Nb points and vehicles"));
        nbInput.add(nodes);
        nbInput.add(nbNodes);
        nbInput.add(vehicles);
        nbInput.add(nbVehicle);
        add(nbInput);
    }

    public void setInstances(){
        instances = new JComboBox(new String[]{"Random","Fixed-1","Fixed-2","Fixed-3","Fixed-4","Fixed-5"});
        instances.setBackground(Color.white);
        JPanel instancesPanel = new JPanel();
        instancesPanel.add(instances);
        instancesPanel.setBackground(Color.white);
        instancesPanel.setBorder(BorderFactory.createTitledBorder("Instances"));

        add(instancesPanel);

        makeInstance = new JButton("Make instance");
        makeInstance.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                VisualDebug.initModel(false);
            }
        });

        makeInstance.setBackground(Color.white);
        add(makeInstance);
    }



    public Boolean inPause(){
        return pause;
    }

    public Boolean inIteration(){
        return iteration;
    }

    public Boolean writeRoute(){
        return writeRoute.isSelected();
    }



}
