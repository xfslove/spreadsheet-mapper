package me.excel.tools.factory;

import me.excel.tools.model.excel.ExcelRow;

/**
 * 自定义的model factory需继承此类
 *
 * Created by hanwen on 15-12-16.
 */
public abstract class AbstractModelFactory implements ModelFactory {

  protected Class modelClazz;

  public AbstractModelFactory(Class modelClazz) {
    this.modelClazz = modelClazz;
  }

  @Override
  public abstract Object create(ExcelRow row);
}