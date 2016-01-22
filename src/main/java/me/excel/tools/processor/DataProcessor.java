package me.excel.tools.processor;

import java.util.List;

/**
 * Created by hanwen on 15-12-16.
 */
public interface DataProcessor {

  void preProcessing(Object model);

  void postProcessing(Object model);

  void handle(List models);
}
