package me.excel.tools.processor;

import me.excel.tools.model.ext.SheetContext;
import me.excel.tools.setter.FieldValueSetter;

import java.util.List;

/**
 * Created by hanwen on 2016/12/28.
 */
public interface ObjectProcessor {

  /**
   * <pre>
   * field value setter unique with object field in one sheet (one to one),
   * if you add setter with same match field({@link FieldValueSetter#getMatchField()}),
   * after add will override before add
   * </pre>
   *
   * @param setters field value setter
   * @see FieldValueSetter
   */
  void addFieldValueSetter(FieldValueSetter... setters);

  /**
   * one sheet, one model factory
   *
   * @param objectFactories model factorys
   * @see ObjectFactory
   */
  void addObjectFactory(ObjectFactory... objectFactories);

  /**
   * one sheet one listener
   *
   * @param objectProcessorListeners listeners
   * @see ObjectProcessorListener
   */
  void addObjectProcessorListener(ObjectProcessorListener... objectProcessorListeners);

  /**
   * @return list of sheets data
   * @see SheetContext
   */
  List<SheetContext> process();
}
