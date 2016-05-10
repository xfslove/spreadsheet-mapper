package me.excel.tools.extractor;

import me.excel.tools.FieldUtils;
import org.apache.commons.beanutils.NestedNullException;
import org.apache.commons.beanutils.PropertyUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.text.SimpleDateFormat;
import java.util.Date;

/**
 * Created by hanwen on 5/5/16.
 */
@Deprecated
public class DateFormatExtractor extends AbstractCellValueExtractor {

  private static final Logger LOGGER = LoggerFactory.getLogger(DateFormatExtractor.class);

  private String pattern;

  public DateFormatExtractor(String matchField, String pattern) {
    super(matchField);
    this.pattern = pattern;
  }

  @Override
  public String getStringValue(Object data, String field) {

    try {
      Date value = (Date) PropertyUtils.getProperty(data, FieldUtils.getFieldWithoutPrefix(field));

      SimpleDateFormat simpleDateFormat = new SimpleDateFormat(pattern);

      return value == null ? null : simpleDateFormat.format(value);

    }catch (NestedNullException e) {
      LOGGER.trace(e.getMessage());
      return null;
    }  catch (Exception e) {
      LOGGER.error(ExceptionUtils.getStackTrace(e));
      throw new IllegalArgumentException(e);
    }
  }
}
