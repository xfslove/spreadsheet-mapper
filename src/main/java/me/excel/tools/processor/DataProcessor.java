package me.excel.tools.processor;

import java.util.List;

/**
 * processor java bean data after transfer from excel
 * <p>
 * Created by hanwen on 15-12-16.
 */
public interface DataProcessor {

  /**
   * before processing set value (value not set)
   *
   * @param origin
   */
  void preProcessing(Object origin);

  /**
   * after processing set value (value set)
   *
   * @param model
   */
  void postProcessing(Object model);

  /**
   * handle value set data list
   *
   * @param models
   */
  void handle(List models);
}
