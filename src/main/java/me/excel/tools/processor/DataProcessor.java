package me.excel.tools.processor;

import java.util.List;

/**
 * processor model data after transfer from excel
 * <p>
 * Created by hanwen on 15-12-16.
 */
public interface DataProcessor {

  /**
   * before processing set value
   *
   * @param origin value not set
   */
  void preProcessing(Object origin);

  /**
   * after processing set value
   *
   * @param model value set
   */
  void postProcessing(Object model);

  /**
   * handle value set model list
   *
   * @param models value set model list
   */
  void handle(List models);
}
