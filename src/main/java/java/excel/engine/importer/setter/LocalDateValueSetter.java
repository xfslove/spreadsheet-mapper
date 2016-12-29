package java.excel.engine.importer.setter;

import java.excel.engine.exception.ExcelProcessException;
import java.excel.engine.model.excel.Cell;
import org.apache.commons.beanutils.PropertyUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.excel.engine.util.FieldUtils;

/**
 * local date field value setter
 * <p>
 * Created by hanwen on 5/3/16.
 */
public class LocalDateValueSetter extends FieldValueSetterAdapter {

  private static final Logger LOGGER = LoggerFactory.getLogger(LocalDateValueSetter.class);

  private String pattern;

  public LocalDateValueSetter(String matchField, String pattern) {
    super(matchField);
    this.pattern = pattern;
  }

  @Override
  public void set(Object data, Cell cell) {
    try {
      DateTimeFormatter dateTimeFormatter = DateTimeFormat.forPattern(pattern);
      String value = cell.getValue();
      PropertyUtils.setProperty(data, FieldUtils.detectRealField(getMatchField()), value == null ? null : dateTimeFormatter.parseLocalDate(value));
    } catch (Exception e) {
      LOGGER.error(ExceptionUtils.getStackTrace(e));
      throw new ExcelProcessException(e);
    }
  }
}
