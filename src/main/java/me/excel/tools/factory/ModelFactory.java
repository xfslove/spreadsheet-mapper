package me.excel.tools.factory;

import me.excel.tools.model.excel.ExcelRow;

/**
 * model factory
 *
 * Created by hanwen on 15-12-16.
 */
public interface ModelFactory {

  Object create(ExcelRow row);
}
