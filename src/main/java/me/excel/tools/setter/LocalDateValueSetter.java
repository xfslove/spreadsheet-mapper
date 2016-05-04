package me.excel.tools.setter;

import me.excel.tools.importer.ExcelImportException;
import me.excel.tools.model.excel.ExcelCell;
import org.apache.commons.beanutils.PropertyUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static me.excel.tools.FieldUtils.getFieldWithoutPrefix;

/**
 * Created by hanwen on 5/3/16.
 */
public class LocalDateValueSetter extends AbstractCellValueSetter {

  private static final Logger LOGGER = LoggerFactory.getLogger(LocalDateValueSetter.class);

  private String pattern;

  public LocalDateValueSetter(String matchField, String pattern) {
    super(matchField);
    this.pattern = pattern;
  }
  @Override
  public void set(Object data, ExcelCell excelCell) {
    try {
      DateTimeFormatter dateTimeFormatter = DateTimeFormat.forPattern(pattern);
      String value = excelCell.getValue();
      PropertyUtils.setProperty(data, getFieldWithoutPrefix(excelCell.getField()), value == null ? null : dateTimeFormatter.parseLocalDate(value));
    } catch (Exception e) {
      LOGGER.error(ExceptionUtils.getStackTrace(e));
      throw new ExcelImportException(e);
    }
  }
}
