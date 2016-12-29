package excel.engine.importer.setter;

import excel.engine.exception.ExcelProcessException;
import excel.engine.model.excel.Cell;
import org.apache.commons.beanutils.PropertyUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import excel.engine.util.FieldUtils;

/**
 * local date time field value setter
 * <p>
 * Created by hanwen on 5/3/16.
 */
public class LocalDateTimeValueSetter extends FieldValueSetterAdapter {

  private static final Logger LOGGER = LoggerFactory.getLogger(LocalDateTimeValueSetter.class);

  private String pattern;

  public LocalDateTimeValueSetter(String matchField, String pattern) {
    super(matchField);
    this.pattern = pattern;
  }

  @Override
  public void set(Object data, Cell cell) {
    try {
      DateTimeFormatter dateTimeFormatter = DateTimeFormat.forPattern(pattern);
      String value = cell.getValue();
      PropertyUtils.setProperty(data, FieldUtils.detectRealField(getMatchField()), value == null ? null : dateTimeFormatter.parseLocalDateTime(value));
    } catch (Exception e) {
      LOGGER.error(ExceptionUtils.getStackTrace(e));
      throw new ExcelProcessException(e);
    }
  }
}
