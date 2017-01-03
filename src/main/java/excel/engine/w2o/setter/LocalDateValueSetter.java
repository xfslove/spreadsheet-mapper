package excel.engine.w2o.setter;

import excel.engine.model.core.Cell;
import excel.engine.model.meta.FieldMeta;
import excel.engine.util.FieldUtils;
import excel.engine.w2o.processor.WorkbookProcessException;
import org.apache.commons.beanutils.PropertyUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * local date field value setter
 * <p>
 * Created by hanwen on 5/3/16.
 */
public class LocalDateValueSetter extends FieldValueSetterAdapter {

  private static final Logger LOGGER = LoggerFactory.getLogger(LocalDateValueSetter.class);

  private String pattern;

  public LocalDateValueSetter(String pattern, String matchField) {
    super(matchField);
    this.pattern = pattern;
  }

  @Override
  public void set(Object data, Cell cell, FieldMeta fieldMeta) {
    try {
      DateTimeFormatter dateTimeFormatter = DateTimeFormat.forPattern(pattern);
      String value = cell.getValue();
      PropertyUtils.setProperty(data, FieldUtils.detectRealField(fieldMeta.getName()), value == null ? null : dateTimeFormatter.parseLocalDate(value));
    } catch (Exception e) {
      LOGGER.error(ExceptionUtils.getStackTrace(e));
      throw new WorkbookProcessException(e);
    }
  }
}
