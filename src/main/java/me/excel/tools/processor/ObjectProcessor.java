package me.excel.tools.processor;

import me.excel.tools.setter.FieldValueSetter;

import java.util.List;

/**
 * Created by hanwen on 2016/12/28.
 */
public interface ObjectProcessor {

  /**
   * @param setters field value setter
   * @see FieldValueSetter
   */
  void addCellValueSetter(FieldValueSetter... setters);

  /**
   * @param objectFactories model factorys
   * @see ObjectFactory
   */
  void addModelFactory(ObjectFactory... objectFactories);

  /**
   * @param objectProcessorListeners listeners
   * @see ObjectProcessorListener
   */
  void addObjectProcessorListener(ObjectProcessorListener... objectProcessorListeners);

  List<List<Object>> process();
}
